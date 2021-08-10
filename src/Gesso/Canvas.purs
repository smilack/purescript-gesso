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
import CSS as CSS
import Data.Foldable (foldr, traverse_)
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
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
import Halogen.HTML (AttrName(..), memoized, canvas, attr)
import Halogen.HTML.Properties (IProp, id, tabIndex)
import Halogen.Query.Event as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
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
_gessoCanvas = Proxy :: Proxy "gessoCanvas"

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
  =
  { name :: String
  , app :: App.Application localState globalState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , clientRect :: Maybe Dims.ClientRect
  , canvas :: Maybe HTMLElement
  , context :: Maybe Context2D
  , scaler :: Maybe Dims.Scaler
  , resizeSub :: Maybe H.SubscriptionId
  , interactions :: GI.Interactions localState (Action localState globalState)
  , queuedInteractions :: List (GI.FullHandler localState)
  , processingInteractions :: List (GI.FullHandler localState)
  , rafId :: Maybe T.RequestAnimationFrameId
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
  | InteractionsProcessed
  | MaybeTick
  | FrameRequested T.RequestAnimationFrameId
  | FrameFired

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
  =
  { name :: String
  , app :: App.Application localState globalState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , interactions :: GI.Interactions localState (Action localState globalState)
  }

-- | Definition of the Canvas component. `render` is memoized so that it only
-- | re-renders when the size of the element changes.
component
  :: forall localState appInput appOutput globalState m
   . MonadAff m
  => ManageState m globalState
  => H.Component (Query appInput) (Input localState globalState appInput appOutput) (Output appOutput) m
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
-- | input. The rest require Effects and are created in `initialize`, except for
-- | the interactions queues, which start empty.
initialState
  :: forall localState globalState appInput appOutput
   . Input localState globalState appInput appOutput
  -> State localState globalState appInput appOutput
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
  , queuedInteractions: List.Nil
  , processingInteractions: List.Nil
  , rafId: Nothing
  }

-- | Canvas component's render function. Size/position CSS comes from
-- | `Application`, event properties come from `Interactions`, and
-- | `width`/`height` attributes come from Dimensions.
-- |
-- | The `width` and `height` attributes may be different from the size CSS. The
-- | CSS controls the size of the element, while the HTML attributes control the
-- | scale of the drawing area.
render
  :: forall localState globalState appInput appOutput slots m
   . State localState globalState appInput appOutput
  -> H.ComponentHTML (Action localState globalState) slots m
render { name, clientRect, app, interactions } =
  canvas
    $ [ id name, style $ App.windowCss app, tabIndex 0 ]
      <> GI.toProps InteractionTriggered interactions
      <> maybe [] Dims.toSizeProps clientRect
  where
  style :: forall r i. CSS.CSS -> IProp (style :: String | r) i
  style =
    attr (AttrName "style")
      <<< fromMaybe ""
      <<< CSS.renderedInline
      <<< CSS.rules []
      <<< CSS.runS

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
-- | - `InteractionTriggered`: An event fired, add the handler to a queue to be
-- |   run on the next animation frame.
-- | - `InteractionsProcessed`: The `processingInteractions` queue was
-- |   successfully processed in an animation frame, so those interactions can
-- |   be discarded.
-- | - `StateUpdated`: The local state is changing. Save it, tell `Application`
-- |   to handle output, and request animation frame if rendering after updates.
-- | - `MaybeTick`: Request an animation frame if the application should render
-- |   after state changes and if a frame has not already been requested.
-- | - `FrameRequested`: An animation frame has been requested, save its ID.
-- | - `FrameFired`: The requested animation frame has fired, forget its ID.
handleAction
  :: forall localState globalState appInput appOutput slots m
   . MonadAff m
  => ManageState m globalState
  => (Action localState globalState)
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
handleAction = case _ of
  Initialize -> do
    initialize
    handleAction $ Tick Nothing
  HandleStateBus globalState' -> do
    { app, localState } <- H.get
    App.receiveGlobal saveLocal localState globalState' app
    handleAction MaybeTick
  HandleResize -> do
    updateClientRect
    handleAction MaybeTick
  Tick mLastTime -> do
    { context, localState, app, scaler, queuedInteractions, processingInteractions } <- H.get
    -- Prepend any newly queued interactions to the list of interactions we've
    --   tried to process, then empty the main queue and replace the processing
    --   queue with the sum of both.
    let
      tryInteractions = queuedInteractions <> processingInteractions
    H.modify_
      ( _
          { queuedInteractions = List.Nil
          , processingInteractions = tryInteractions
          }
      )
    queueAnimationFrame mLastTime context scaler tryInteractions localState app
  Finalize -> unsubscribeResize
  -- If effectful interactions become necessary, handlerFn could return
  --   m (Maybe localState) - Just if localState is changed, or Nothing if
  --   localState is not changed. Probably should just be Effect because
  --   ManageState is getting complicated with addition of OutputStyles
  -- Hold on to interactions until the next tick, then pass them into rAF
  InteractionTriggered handlerFn -> do
    queuedInteractions <- H.gets _.queuedInteractions
    H.modify_ (_ { queuedInteractions = handlerFn : queuedInteractions })
    handleAction MaybeTick
  -- The interactions passed into an animation frame have been processed and are
  --   no longer needed.
  InteractionsProcessed -> H.modify_ (_ { processingInteractions = List.Nil })
  StateUpdated localState' -> do
    modifyState localState'
    handleAction MaybeTick
  MaybeTick -> do
    { app, rafId } <- H.get
    case App.renderOnUpdate app of
      App.Stop -> pure unit
      App.Continue -> case rafId of -- don't need to tick if a frame was already requested
        Nothing -> handleAction $ Tick Nothing
        Just _ -> pure unit
  FrameRequested rafId -> H.modify_ (_ { rafId = Just rafId })
  FrameFired -> H.modify_ (_ { rafId = Nothing })

-- | Subscribe to the global state bus and window resize events. Get the
-- | `canvas` element and its `Context2D` and `clientRect`. Create scaling
-- | functions based on the `viewBox` and `clientRect`.
initialize
  :: forall localState globalState appInput appOutput slots output m
   . MonadAff m
  => ManageState m globalState
  => H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m Unit
initialize = do
  stateEmitter <- GM.getEmitter
  _ <- H.subscribe $ HandleStateBus <$> stateEmitter
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
-- | First, create a paired emitter and listener. Subscribe to the emitter, and
-- | call `requestAnimationFrame` with `rafCallback`. `rafCallback` is a
-- | closure so that it has access to values from the component state.
-- | `requestAnimationFrame` returns an ID which we send to the component via
-- | the listener and the `FrameRequested` action.
-- |
-- | `rafCallback` emits a `FrameFired` action indicating the callback started.
-- | Next, it calculates the time delta based on the current and previous
-- | timestamps. If there is no previous timestamp, the `updateAndRender` call
-- | is skipped and another frame requested.
-- |
-- | The `updateAndRender` call could also be skipped because the client rect
-- | hasn't been found, so there is no scaler, or the canvas element hasn't
-- | been found, so there is no context.
-- |
-- | `updateAndRender` converts the list of queued interaction handlers to
-- | `localState -> Maybe localState` functions, and prepends a mostly filled-in
-- | call to `App.updateLocalState` with the same type. This list is folded
-- | over, keeping track of whether the state is changed by any of the
-- | functions. If it is, a `StateUpdated` action is emitted. Finally, the
-- | state (updated or not) is passed to `App.renderApp`.
-- |
-- | `App.renderApp` returns a value instructing whether to request another
-- | frame, which is passed out to `rafCallback`. It varies based on the
-- | `RenderMode` of the application. If the application renders continuously
-- | then it emits a `Tick` action. If it renders on update then it does not.
-- | It also continues if `updateAndRender` was skipped because any values were
-- | `Nothing`, in the hope that the missing values will eventually appear.
queueAnimationFrame
  :: forall localState globalState appInput appOutput slots output m
   . MonadAff m
  => Maybe (T.TimestampPrevious)
  -> Maybe Context2D
  -> Maybe Dims.Scaler
  -> List (GI.FullHandler localState)
  -> localState
  -> App.Application localState globalState appInput appOutput
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m Unit
queueAnimationFrame mLastTime mcontext mscaler queuedInteractions localState app = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.subscribe emitter
  H.liftEffect do
    wnd <- window
    rafId <- T.requestAnimationFrame (rafCallback listener) wnd
    HS.notify listener $ FrameRequested rafId
  pure unit
  where
  rafCallback
    :: HS.Listener (Action localState globalState)
    -> T.TimestampCurrent
    -> Effect Unit
  rafCallback listener timestamp = do
    HS.notify listener FrameFired
    anotherFrame <-
      map join $ sequence $ updateAndRender listener
        <$> (T.delta timestamp <$> mLastTime)
        <*> mcontext
        <*> mscaler
    case anotherFrame of
      Just App.Stop -> pure unit
      Just App.Continue -> HS.notify listener $ Tick $ Just $ T.toPrev timestamp
      Nothing -> HS.notify listener $ Tick $ Just $ T.toPrev timestamp --try again in case we haven't gotten a delta or context yet

  updateAndRender
    :: HS.Listener (Action localState globalState)
    -> T.Delta
    -> Context2D
    -> Dims.Scaler
    -> Effect (Maybe App.RequestFrame)
  updateAndRender listener delta context scaler = do
    let
      -- flap (<@>) applies delta and scaler to every function in
      --   queuedInteractions
      qIs = queuedInteractions <@> delta <@> scaler

      qIsThenUpdate = (\s -> App.updateLocalState delta scaler s app) : qIs

      changed /\ state' = foldr applyUpdateFn (DidNotChange /\ localState) qIsThenUpdate
    case changed of
      Changed -> HS.notify listener $ StateUpdated state'
      DidNotChange -> pure unit
    HS.notify listener InteractionsProcessed
    sequence $ App.renderApp state' delta scaler context app

  applyUpdateFn
    :: (localState -> Maybe localState)
    -> Tuple StateChanged localState
    -> Tuple StateChanged localState
  applyUpdateFn handlerFn s@(_ /\ state) = case handlerFn state of
    Just state' -> Changed /\ state'
    Nothing -> s

-- | A simple type to track whether the local state changed while running
-- | interactions and updates.
data StateChanged
  = Changed
  | DidNotChange

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
updateClientRect
  :: forall localState globalState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState globalState appInput appOutput) action slots output m Unit
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
unsubscribeResize
  :: forall localState globalState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState globalState appInput appOutput) action slots output m Unit
unsubscribeResize = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub

-- | Subscribe to window resize events and fire the `HandleResize` `Action` when
-- | they occur.
subscribeResize
  :: forall localState globalState appInput appOutput slots output m
   . MonadAff m
  => H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots output m H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ HE.eventListener
      (EventType "resize")
      (toEventTarget wnd)
      (const $ Just HandleResize)

-- | Attempt to raise an output message. Called in `Application.handleOutput`,
-- | which is called when local state changes. Only relevant if `OutputMode` is
-- | `OutputFn`, and the output function may return `Nothing` if the change in
-- | local state is not worth outputting.
sendOutput
  :: forall localState globalState appInput appOutput slots m
   . MonadAff m
  => ManageState m globalState
  => Maybe appOutput
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
sendOutput = traverse_ (H.raise <<< Output)

-- | Save a new version of the application's local state to the component's
-- | state. This is also passed to `Application.receiveInput` to simplify the
-- | process of receiving input and modifying the state accordingly.
saveLocal
  :: forall localState globalState appInput appOutput slots m
   . MonadAff m
  => ManageState m globalState
  => localState
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
saveLocal state' = H.modify_ (_ { localState = state' })

-- | Modify the local state of the application. The new state is saved, and the
-- | new and old states are given to `Application` to check the `OutputMode` and
-- | send output, if necessary.
modifyState
  :: forall localState globalState appInput appOutput slots m
   . MonadAff m
  => ManageState m globalState
  => localState
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m Unit
modifyState state' = do
  { app, localState } <- H.get
  saveLocal state'
  App.handleOutput sendOutput localState state' app

-- | Handle `Input` from the host application according to the `Application`'s
-- | `InputReceiver` function, and request an animation frame if rendering after
-- | state updates.
handleQuery
  :: forall localState globalState appInput appOutput slots a m
   . MonadAff m
  => ManageState m globalState
  => Query appInput a
  -> H.HalogenM (State localState globalState appInput appOutput) (Action localState globalState) slots (Output appOutput) m (Maybe a)
handleQuery (Input inData a) = do
  { app, localState } <- H.get
  App.receiveInput saveLocal localState inData app
  handleAction MaybeTick
  pure (Just a)
