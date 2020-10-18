module Gesso.Canvas
  ( component
  , Input
  , Action
  , Output(..)
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

type Slot output slot
  = forall q. H.Slot q (Output output) slot

_gessoCanvas = SProxy :: SProxy "gessoCanvas"

type State localState appOutput globalState
  = { name :: String
    , app :: App.Application localState appOutput globalState
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

type Input localState appOutput globalState
  = { name :: String
    , app :: App.Application localState appOutput globalState
    , localState :: localState
    , viewBox :: Dims.ViewBox
    , interactions :: GI.Interactions localState (Action localState globalState)
    }

component ::
  forall localState query appOutput globalState m.
  MonadAff m =>
  ManageState m globalState =>
  H.Component HTML query (Input localState appOutput globalState) (Output appOutput) m
component =
  H.mkComponent
    { initialState
    , render: memoized (eq `on` _.clientRect) render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , finalize = Just Finalize
              }
    }

initialState ::
  forall localState appOutput globalState.
  Input localState appOutput globalState ->
  State localState appOutput globalState
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
  forall localState appOutput globalState slots m.
  State localState appOutput globalState ->
  H.ComponentHTML (Action localState globalState) slots m
render { name, clientRect, app, interactions } =
  canvas
    $ [ id_ name, style $ App.windowCss app ]
    <> GI.toProps (Just <<< InteractionTriggered) interactions
    <> maybe [] Dims.toSizeProps clientRect

handleAction ::
  forall localState appOutput globalState slots m.
  MonadAff m =>
  ManageState m globalState =>
  (Action localState globalState) ->
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots (Output appOutput) m Unit
handleAction = case _ of
  Initialize -> do
    initialize
    handleAction $ Tick Nothing
  HandleStateBus globalState' -> do
    { app, localState } <- H.get
    let
      localState' = App.updateLocal localState globalState' app
    H.modify_ (_ { localState = localState' })
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
  forall localState appOutput globalState slots output m.
  MonadAff m =>
  ManageState m globalState =>
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots output m Unit
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
  forall localState appOutput globalState slots output m.
  MonadAff m =>
  Maybe (T.TimestampPrevious) ->
  Maybe Context2D ->
  Maybe Dims.Scaler ->
  localState ->
  App.Application localState appOutput globalState ->
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots output m Unit
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
  forall localState appOutput globalState action slots output m.
  MonadAff m =>
  H.HalogenM (State localState appOutput globalState) action slots output m Unit
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
  forall localState appOutput globalState action slots output m.
  MonadAff m =>
  H.HalogenM (State localState appOutput globalState) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub

subscribeResize ::
  forall localState appOutput globalState slots output m.
  MonadAff m =>
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots output m H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ ES.eventListenerEventSource
        (EventType "resize")
        (toEventTarget wnd)
        (const $ Just HandleResize)

sendOutput ::
  forall localState appOutput globalState slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  Maybe appOutput ->
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots (Output appOutput) m Unit
sendOutput state' moutput = do
  H.modify_ (_ { localState = state' })
  traverse_ (H.raise <<< Output) moutput

-- I think I need to be able to make ManageState's state and localState different. What would that take?
modifyState ::
  forall localState appOutput globalState slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  H.HalogenM (State localState appOutput globalState) (Action localState globalState) slots (Output appOutput) m Unit
modifyState state' = do
  { app, localState } <- H.get
  App.handleOutput sendOutput localState state' app
