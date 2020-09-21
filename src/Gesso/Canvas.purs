module Gesso.Canvas where

import Prelude
import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse, sequence)
import Debug.Trace (trace, traceM, spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as App
import Gesso.Dimensions as Dims
import Gesso.Time as T
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML (HTML, memoized, canvas)
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (id_)
import Halogen.Query.EventSource as ES
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (getBoundingClientRect, fromElement, HTMLElement, DOMRect)
import Web.HTML.Window (toEventTarget, document)
import Web.UIEvent.MouseEvent (MouseEvent)

type State appState
  = { name :: String
    , app :: App.Application appState
    , appState :: appState
    , viewBox :: Dims.ViewBox
    , clientRect :: Maybe Dims.ClientRect
    , canvas :: Maybe HTMLElement
    , context :: Maybe Context2D
    , scaler :: Maybe Dims.Scaler
    , resizeSub :: Maybe H.SubscriptionId
    }

data Query appState a
  = UpdateAppState appState a

data Action appState
  = Initialize
  | Finalize
  | HandleResize
  | Tick (Maybe (T.Timestamp T.Prev))
  | StateUpdatedByApp appState
  | MouseMoveEvent Dims.Point

newtype Input appState
  = Input
  { name :: String
  , app :: App.Application appState
  , appState :: appState
  , viewBox :: Dims.ViewBox
  }

data Output appState
  = StateUpdated appState
  | MouseMove Dims.Point

component :: forall appState m. MonadAff m => H.Component HTML (Query appState) (Input appState) (Output appState) m
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

initialState :: forall appState. Input appState -> State appState
initialState (Input { name, app, appState, viewBox }) =
  { name
  , app
  , appState
  , viewBox
  , clientRect: Nothing
  , canvas: Nothing
  , context: Nothing
  , scaler: Nothing
  , resizeSub: Nothing
  }

render :: forall appState slots m. State appState -> H.ComponentHTML (Action appState) slots m
render { name, clientRect, app } =
  canvas
    $ [ id_ name
      , HE.onMouseMove getCursorCoordinates
      , style $ App.windowCss app
      ]
    <> maybe [] Dims.toSizeProps clientRect

getCursorCoordinates :: forall appState. MouseEvent -> Maybe (Action appState)
getCursorCoordinates = Just <<< MouseMoveEvent <<< Dims.fromMouseEvent

handleQuery ::
  forall appState a slots output m.
  MonadAff m =>
  Query appState a ->
  H.HalogenM (State appState) (Action appState) slots output m (Maybe a)
handleQuery (UpdateAppState appState a) = do
  H.modify_ (_ { appState = appState })
  pure $ Just a

handleAction ::
  forall appState slots m.
  MonadAff m =>
  (Action appState) ->
  H.HalogenM (State appState) (Action appState) slots (Output appState) m Unit
handleAction = case _ of
  Initialize -> do
    initialize
    handleAction $ Tick Nothing
  HandleResize -> updateClientRect
  Tick mLastTime -> do
    { context, appState, app, scaler } <- H.get
    queueAnimationFrame mLastTime context scaler appState app
  Finalize -> unsubscribeResize
  StateUpdatedByApp appState -> do
    H.modify_ (_ { appState = appState })
    H.raise $ StateUpdated appState
  MouseMoveEvent p -> H.raise $ MouseMove p

initialize :: forall appState slots output m. MonadAff m => H.HalogenM (State appState) (Action appState) slots output m Unit
initialize = do
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
  forall appState slots output m.
  MonadAff m =>
  Maybe (T.Timestamp T.Prev) ->
  Maybe Context2D ->
  Maybe Dims.Scaler ->
  appState ->
  App.Application appState ->
  H.HalogenM (State appState) (Action appState) slots output m Unit
queueAnimationFrame mLastTime context scaler appState app = do
  _ <- H.subscribe $ ES.effectEventSource rafEventSource
  pure unit
  where
  rafEventSource :: ES.Emitter Effect (Action appState) -> Effect (ES.Finalizer Effect)
  rafEventSource emitter = do
    _ <- T.requestAnimationFrame (rafCallback emitter) =<< window
    mempty

  rafCallback :: ES.Emitter Effect (Action appState) -> T.Timestamp T.Now -> Effect Unit
  rafCallback emitter timestamp = do
    let
      mdelta = T.delta timestamp <$> mLastTime

      mstate = join $ App.updateAppState <$> mdelta <*> pure appState <*> pure app
    traverse_ (ES.emit emitter <<< StateUpdatedByApp) mstate
    anotherFrame <-
      sequence $ join
        $ App.renderApp
        <$> (mstate <|> pure appState)
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
  forall appState action slots output m.
  MonadAff m =>
  H.HalogenM (State appState) action slots output m Unit
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
  forall appState action slots output m.
  MonadAff m =>
  H.HalogenM (State appState) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub

subscribeResize ::
  forall appState slots output m.
  MonadAff m =>
  H.HalogenM (State appState) (Action appState) slots output m H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ ES.eventListenerEventSource
        (EventType "resize")
        (toEventTarget wnd)
        (const $ Just HandleResize)
