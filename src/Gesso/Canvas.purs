-- | Gesso Canvas is a Halogen component that handles creating a canvas element,
-- | calling requestAnimationFrame, attaching events, and running render and
-- | update functions.
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

-- | The Halogen slot type for Canvas, which is used to include it inside
-- | another Halogen component.
type Slot input output slot
  = H.Slot (Query input) (Output output) slot

-- | A proxy type for Canvas provided for convenience, for use with Slot.
_gessoCanvas = SProxy :: SProxy "gessoCanvas"

-- | The internal state of the Canvas component
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `app` is the Application spec.
-- | - `localState` is the local state of the application.
-- | - `viewBox` is the position and dimensions of the drawing surface.
-- | - `clientRect` is the actual position and dimensions of the canvas element.
-- | - `canvas` is the canvas element.
-- | - `context` is the `Context2D` for the canvas element.
-- | - `scaler` is the scaler record that converts between viewbox and client
-- |   rect coordinates.
-- | - `resizeSub` is a subscription to window resize events, to re-get the
-- |   `clientRect` and recreate the `scaler`.
-- | - `interactions` is the events attached to the canvas element.
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

-- | See [`handleAction`](#v:handleAction)
data Action localState globalState
  = Initialize
  | HandleStateBus globalState
  | HandleResize
  | Tick (Maybe T.TimestampPrevious)
  | Finalize
  | StateUpdated localState
  | InteractionTriggered (GI.FullHandler localState)
  | MaybeTick

-- | The component's output type is defined by the `OutputMode` in the
-- | `Application` spec.
newtype Output appOutput
  = Output appOutput

-- | The component's input type is defined by the `InputReceiver` in the
-- | `Application` spec.
data Query appInput a
  = Input appInput a

-- | The input provided when the Canvas component is created. Canvas has no
-- | `receive` in its `EvalSpec`, so the input is only read once.
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `app` is the Application spec.
-- | `localState` is the initial local state for the application.
-- | `viewBox` is the desired dimensions for the drawing surface.
-- | `interactions` is the events which will be attached to the canvas element.
type Input localState globalState appInput appOutput
  = { name :: String
    , app :: App.Application localState globalState appInput appOutput
    , localState :: localState
    , viewBox :: Dims.ViewBox
    , interactions :: GI.Interactions localState (Action localState globalState)
    }

-- | Definition of the Canvas component. `render` is memoized so that it only
-- | re-renders when the size of the element changes.
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

-- | Get initial state for Canvas. Most values are copied directly from the
-- | input. The rest require Effects and are created in `initialize`.
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

-- | Canvas component's render function. Size/position CSS comes from
-- | `Application`, event properties come from `Interactions`, and
-- | `width`/`height` attributes come from Dimensions.
-- |
-- | The `width` and `height` attributes may be different from the size CSS. The
-- | CSS controls the size of the element, while the HTML attributes control the
-- | scale of the drawing area.
render ::
  forall localState globalState appInput appOutput slots m.
  State localState globalState appInput appOutput ->
  H.ComponentHTML (Action localState globalState) slots m
render { name, clientRect, app, interactions } =
  canvas
    $ [ id_ name, style $ App.windowCss app ]
    <> GI.toProps (Just <<< InteractionTriggered) interactions
    <> maybe [] Dims.toSizeProps clientRect

-- | - `Initialize`: Create `context`, `resizeSub`, `clientRect`, `canvas`, and
-- |   `scaler` values. Then recurse with `Tick` to request the first animation
-- |   frame.
-- | - `HandleStateBus`: New global received on state bus, process it and
-- |   request animation frame if rendering after updates.
-- | - `HandleResize`: Window resized, get new client rect and recalculate
-- |   `scaler` functions.
-- | - `Tick`: Request an animation frame. When animating, `Tick` passes the
-- |   animation frame timestamp to itself so it can calculate the delta between
-- |   frames.
-- | - `Finalize`: Unsubscribe to window resize events.
-- | - `InteractionTriggered`: An event fired, call the handler and update the
-- |   state if necessary.
-- | - `StateUpdated`: The local state is changing. Save it, tell `Application`
-- |   to handle output, and request animation frame if rendering after updates.
-- | - `MaybeTick`: Request an animation frame if the application should render
-- |   after state updates.
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
  -- If effectful interactions become necessary, handlerFn could return
  --   m (Maybe localState) - Just if localState is changed, or Nothing if
  --   localState is not changed. Probably should just be Effect because
  --   ManageState is getting complicated with addition of OutputStyles
  InteractionTriggered handlerFn -> do
    { scaler, localState } <- H.get
    case scaler >>= \s -> handlerFn s localState of
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

-- | Subscribe to the global state bus and window resize events. Get the
-- | `canvas` element and its `Context2D` and `clientRect`. Create scaling
-- | functions based on the `viewBox` and `clientRect`.
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

-- | Runs update and render functions:
-- |
-- | First, create and subscribe to an `effectEventSource` which calls
-- | `requestAnimationFrame` with `rafCallback`. `rafCallback` is a closure so
-- | that it has access to values from the component state.
-- |
-- | `rafCallback` first calculates the delta based on the current and previous
-- | timestamps. If there is no previous timestamp, the delta is `Nothing` which
-- | results in skipping the update and render functions and requesting another
-- | frame.
-- |
-- | If there is a previous timestamp, then the delta, application state, and
-- | application spec are passed to Application to run the update function. If
-- | the state updates successfully, a `StateUpdated` action is emitted.
-- |
-- | Next, the render function is called. This could fail for several reasons:
-- | there is no previous timestamp, so no delta; the client rect hasn't been
-- | fount, so no scaler; the canvas element hasn't been found, so no context.
-- |
-- | `renderApp` returns a value instructing whether to request another frame.
-- | This varies based on the `RenderMode` of the application and it does so by
-- | emitting a `Tick` action. If the application renders continuously, then it
-- | ticks does; if it renders on update then it does not. It also continues to
-- | request frames if any values were `Nothing`, in the hope that the missing
-- | values will eventually appear.
-- |
-- | Finally, the `effectEventSource` is closed.
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

-- | Attempt to get the `Context2D` for this component's `canvas` element.
getContext :: String -> Effect (Maybe Context2D)
getContext name = do
  mcanvas <- getCanvasElementById name
  mcontext <- traverse getContext2D mcanvas
  pure mcontext

-- | Attempt to find the `canvas` element on the page.
getCanvasElement :: String -> Effect (Maybe HTMLElement)
getCanvasElement name = do
  doc <- document =<< window
  mcanvas <- getElementById name $ toNonElementParentNode doc
  pure $ mcanvas >>= fromElement

-- | Attempt to get the bounding client rect for an HTML element and convert it
-- | to a `ClientRect` value.
getCanvasClientRect :: Maybe HTMLElement -> Effect (Maybe Dims.ClientRect)
getCanvasClientRect mcanvas = do
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvas
  pure $ Dims.fromDOMRect <$> mbounding

-- | Get a new `clientRect` for the `canvas` element and create a new scaler for
-- | it, saving both to the component state.
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

-- | Unsubscribe to window resize events.
unsubscribeResize ::
  forall localState globalState appInput appOutput action slots output m.
  MonadAff m =>
  H.HalogenM (State localState globalState appInput appOutput) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub

-- | Subscribe to window resize events and fire the `HandleResize` `Action` when
-- | they occur.
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

-- | Attempt to raise an output message. Called in `Application.handleOutput`,
-- | which is called when local state changes. Only relevant if `OutputMode` is
-- | `OutputFn`, and the output function may return `Nothing` if the change in
-- | local state is not worth outputting.
sendOutput ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  Maybe appOutput ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
sendOutput = traverse_ (H.raise <<< Output)

-- | Save a new version of the application's local state to the component's
-- | state. This is also passed to `Application.receiveInput` to simplify the
-- | process of receiving input and modifying the state accordingly.
saveLocal ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
saveLocal state' = H.modify_ (_ { localState = state' })

-- | Modify the local state of the application. The new state is saved, and the
-- | new and old states are given to `Application` to check the `OutputMode` and
-- | send output, if necessary.
modifyState ::
  forall localState globalState appInput appOutput slots m.
  MonadAff m =>
  ManageState m globalState =>
  localState ->
  H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
modifyState state' = do
  { app, localState } <- H.get
  saveLocal state'
  App.handleOutput sendOutput localState state' app

-- | Handle `Input` from the host application according to the `Application`'s
-- | `InputReceiver` function, and request an animation frame if rendering after
-- | state updates.
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
