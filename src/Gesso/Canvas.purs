module Gesso.Canvas
  ( component
  , Input
  , Action
  , Output(..)
  , Query(..)
  , Slot
  , _gessoCanvas
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as App
import Gesso.Dimensions as Dims
import Gesso.GessoM (class ManageState)
import Gesso.GessoM as GM
import Gesso.Interactions as GI
import Gesso.Time as T
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML (HTML, memoized, canvas)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties (id_)
import Halogen.Query.EventSource as ES
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (getBoundingClientRect, fromElement, HTMLElement, DOMRect)
import Web.HTML.Window (toEventTarget, document)

type Slot input output slot
  = H.Slot (Query input) (Output output) slot

_gessoCanvas = SProxy :: SProxy "gessoCanvas"

type State localState globalState appInput appOutput
  = { name :: String
    , app :: App.Application localState globalState appInput appOutput
    , localState :: localState
    , viewBox :: Dims.ViewBox
    , clientRect :: Maybe Dims.ClientRect
    , canvas :: Maybe HTMLElement
    , context :: Maybe Context2D
    , scaler :: Maybe Dims.Scaler
    , resizeSub :: Maybe H.SubscriptionId
    , interactions :: GI.Interactions localState (Action localState globalState)
    }

data Action localState globalState
  = Initialize
  | HandleStateBus globalState
  | HandleResize
  | Tick (Maybe T.TimestampPrevious)
  | Finalize
  | StateUpdated localState
  | InteractionTriggered (GI.FullHandler localState)
  | MaybeTick

newtype Output appOutput
  = Output appOutput

data Query appInput a
  = Input appInput a

type Input localState globalState appInput appOutput
  = { name :: String
    , app :: App.Application localState globalState appInput appOutput
    , localState :: localState
    , viewBox :: Dims.ViewBox
    , interactions :: GI.Interactions localState (Action localState globalState)
    }

component ::
  forall localState appInput appOutput globalState m.
  MonadAff m =>
  ManageState m globalState =>
  H.Component HTML (Query appInput) (Input localState globalState appInput appOutput) (Output appOutput) m
component =
  H.mkComponent
    { initialState
    , render: memoized (eq `on` _.clientRect) render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState ::
  forall localState globalState appInput appOutput.
  Input localState globalState appInput appOutput ->
  State localState globalState appInput appOutput
initialState { name, app, localState, viewBox, interactions } =
  { name
  , app
  , localState
  , viewBox
  , clientRect: Nothing
  , canvas: Nothing
  , context: Nothing
  , scaler: Nothing
  , resizeSub: Nothing
  , interactions
  }

render ::
  forall localState globalState appInput appOutput slots m.
  State localState globalState appInput appOutput ->
  H.ComponentHTML (Action localState globalState) slots m
render { name, clientRect, app, interactions } =
  canvas
    $ [ id_ name, style $ App.windowCss app ]
    <> GI.toProps (Just <<< InteractionTriggered) interactions
    <> maybe [] Dims.toSizeProps clientRect

handleAction ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  (Action localState globalState) ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
handleAction = case _ of
  Initialize -> do
    initialize
    handleAction $ Tick Nothing
  HandleStateBus globalState' -> do
    { app, localState } <- H.get
    App.receiveGlobal saveLocal localState globalState' app
    handleAction MaybeTick
  HandleResize -> updateClientRect
  Tick mLastTime -> do
    { context, localState, app, scaler } <- H.get
    queueAnimationFrame mLastTime context scaler localState app
  Finalize -> unsubscribeResize
  -- If effectful interactions become necessary, updateFn could return
  --   m (Maybe localState) - Just if localState is changed, or Nothing if
  --   localState is not changed. Probably should just be Effect because
  --   ManageState is getting complicated with addition of OutputStyles
  InteractionTriggered updateFn -> do
    { scaler, localState } <- H.get
    case scaler >>= \s -> updateFn s localState of
      Nothing -> pure unit
      Just localState' -> handleAction $ StateUpdated localState'
  StateUpdated localState' -> do
    modifyState localState'
    handleAction MaybeTick
  MaybeTick -> do
    app <- H.gets _.app
    case App.renderOnUpdate app of
      App.Stop -> pure unit
      App.Continue -> handleAction $ Tick Nothing

initialize ::
  forall localState globalState appInput appOutput slots output m.
  MonadAff m =>
  ManageState m globalState =>
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m Unit
initialize = do
  stateEventSource <- GM.getEventSource
  _ <- H.subscribe $ HandleStateBus <$> stateEventSource
  resizeSub <- subscribeResize
  { name, viewBox } <- H.get
  mcontext <- H.liftEffect $ getContext name
  mcanvas <- H.liftEffect $ getCanvasElement name
  clientRect <- H.liftEffect $ getCanvasClientRect mcanvas
  H.modify_
    ( _
        { context = mcontext
        , resizeSub = Just resizeSub
        , clientRect = clientRect
        , canvas = mcanvas
        , scaler = Dims.mkScaler viewBox <$> clientRect
        }
    )

queueAnimationFrame ::
  forall localState globalState appInput appOutput slots output m.
  MonadAff m =>
  Maybe (T.TimestampPrevious) ->
  Maybe Context2D ->
  Maybe Dims.Scaler ->
  localState ->
  App.Application localState globalState appInput appOutput ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m Unit
queueAnimationFrame mLastTime context scaler localState app = do
  _ <- H.subscribe $ ES.effectEventSource rafEventSource
  pure unit
  where
  rafEventSource :: ES.Emitter Effect (Action localState globalState) -> Effect (ES.Finalizer Effect)
  rafEventSource emitter = do
    _ <- T.requestAnimationFrame (rafCallback emitter) =<< window
    mempty

  rafCallback :: ES.Emitter Effect (Action localState globalState) -> T.TimestampCurrent -> Effect Unit
  rafCallback emitter timestamp = do
    let
      mdelta = T.delta timestamp <$> mLastTime

      mstate = join $ App.updateLocalState <$> mdelta <*> pure localState <*> pure app
    traverse_ (ES.emit emitter <<< StateUpdated) mstate
    anotherFrame <-
      sequence $ join
        $ App.renderApp
        <$> (mstate <|> pure localState)
        <*> mdelta
        <*> scaler
        <*> context
        <*> pure app
    case anotherFrame of
      Just App.Stop -> pure unit
      Just App.Continue -> ES.emit emitter $ Tick $ Just $ T.toPrev timestamp
      Nothing -> ES.emit emitter $ Tick $ Just $ T.toPrev timestamp --try again in case we haven't gotten a delta or context yet
    ES.close emitter

getContext :: String -> Effect (Maybe Context2D)
getContext name = do
  mcanvas <- getCanvasElementById name
  mcontext <- traverse getContext2D mcanvas
  pure mcontext

getCanvasElement :: String -> Effect (Maybe HTMLElement)
getCanvasElement name = do
  doc <- document =<< window
  mcanvas <- getElementById name $ toNonElementParentNode doc
  pure $ mcanvas >>= fromElement

getCanvasClientRect :: Maybe HTMLElement -> Effect (Maybe Dims.ClientRect)
getCanvasClientRect mcanvas = do
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvas
  pure $ Dims.fromDOMRect <$> mbounding

updateClientRect ::
  forall localState globalState appInput appOutput action slots output m.
  MonadAff m =>
  H.HalogenM (State localState globalState appInput appOutput) action slots output m Unit
updateClientRect = do
  { canvas, viewBox } <- H.get
  clientRect <- H.liftEffect $ getCanvasClientRect canvas
  H.modify_
    ( _
        { clientRect = clientRect
        , scaler = Dims.mkScaler viewBox <$> clientRect
        }
    )

unsubscribeResize ::
  forall localState globalState appInput appOutput action slots output m.
  MonadAff m =>
  H.HalogenM (State localState globalState appInput appOutput) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub

subscribeResize ::
  forall localState globalState appInput appOutput slots output m.
  MonadAff m =>
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ ES.eventListenerEventSource
        (EventType "resize")
        (toEventTarget wnd)
        (const $ Just HandleResize)

sendOutput ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  Maybe appOutput ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
sendOutput = traverse_ (H.raise <<< Output)

saveLocal ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
saveLocal state' = H.modify_ (_ { localState = state' })

modifyState ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
modifyState state' = do
  { app, localState } <- H.get
  App.handleOutput saveLocal sendOutput localState state' app

handleQuery ::
  forall localState globalState appInput appOutput slots a m.
  MonadAff m =>
  ManageState m globalState =>
  Query appInput a ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m (Maybe a)
handleQuery (Input inData a) = do
  { app, localState } <- H.get
  App.receiveInput saveLocal localState inData app
  handleAction MaybeTick
  pure (Just a)
