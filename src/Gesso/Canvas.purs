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
import Gesso.Interactions as GI
import Gesso.Time as T
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D)
import Halogen as H
import Halogen.HTML (AttrName(..), memoized, canvas, attr)
import Halogen.HTML.Properties (IProp, id, tabIndex)
import Halogen.Query.Event as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.DOM.Element (Element, DOMRect, getBoundingClientRect)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
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
-- | - `app` is the Application Spec.
-- | - `localState` is the local state of the application.
-- | - `viewBox` is the position and dimensions of the drawing surface.
-- | - `clientRect` is the actual position and dimensions of the canvas element.
-- | - `canvas` is the canvas element.
-- | - `context` is the `Context2D` for the canvas element.
-- | - `scaler` is the scaler record that converts between viewbox and client
-- |   rect coordinates.
-- | - `resizeSub` is a subscription to window resize events, to re-get the
-- |   `clientRect` and recreate the `scaler`.
-- | - `emitterSub` is a subscription to the `listener`.
-- | - `listener` is part of a paired listener/emitter used to send Actions from
-- |   `requestAnimationFrame` callbacks to the component.
-- | - `interactions` is the events attached to the canvas element.
-- | - `queuedUpdates` is a list of interactions and Query inputs waiting to be
-- |   applied.
-- | - `processingUpdates` is a list of interactions and Query inputs that have
-- |   been passed to the animation frame to be applied, but which may not have
-- |   been applied yet.
-- | - `rafId` is the ID of the most recently requested animation frame. It's
-- |   set when `requestAnimationFrame` is called and cleared when the animation
-- |   frame callback runs.
type State localState appInput appOutput
  =
  { name :: String
  , app :: App.AppSpec Context2D localState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , clientRect :: Maybe Dims.ClientRect
  , canvas :: Maybe Element
  , context :: Maybe Context2D
  , scaler :: Maybe Dims.Scaler
  , resizeSub :: Maybe H.SubscriptionId
  , emitterSub :: Maybe H.SubscriptionId
  , listener :: Maybe (HS.Listener (Action localState))
  , interactions :: GI.Interactions localState (Action localState)
  , queuedUpdates :: List (App.Update localState)
  , processingUpdates :: List (App.Update localState)
  , rafId :: Maybe T.RequestAnimationFrameId
  }

-- | See [`handleAction`](#v:handleAction)
data Action localState
  = Initialize
  | HandleResize
  | Tick (Maybe T.TimestampPrevious)
  | Finalize
  | StateUpdated T.Delta Dims.Scaler localState
  | QueueUpdate (App.Update localState)
  | UpdatesProcessed
  | FrameRequested T.RequestAnimationFrameId
  | FrameFired

-- | The component's output type is defined by the `OutputMode` in the
-- | `Application.AppSpec`.
newtype Output appOutput
  = Output appOutput

-- | The component's input type is defined by the `InputReceiver` in the
-- | `Application.AppSpec`.
data Query appInput a
  = Input appInput a

-- | The input provided when the Canvas component is created. The component has
-- | no `receive` defined in its `EvalSpec` (see [`component`](#v:component)'s
-- | use of `defaultEval`), so this input is only read once.
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element. (Related:
-- |   https://github.com/smilack/purescript-gesso/issues/4)
-- | - `app` is the Application Spec.
-- | - `localState` is the initial local state for the application.
-- | - `viewBox` is the desired dimensions for the drawing surface.
-- | - `interactions` is the events which will be attached to the
-- |    canvas element.
type Input localState appInput appOutput
  =
  { name :: String
  , app :: App.AppSpec Context2D localState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , interactions :: GI.Interactions localState (Action localState)
  }

-- | Definition of the Canvas component. `render` is memoized so that it only
-- | re-renders when the size of the element changes.
component
  :: forall localState appInput appOutput m
   . MonadAff m
  => H.Component (Query appInput) (Input localState appInput appOutput) (Output appOutput) m
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
-- | input. The rest require Effects and are created in
-- | [`initialize`](#v:initialize), except for the update queues, which start
-- | empty.
initialState
  :: forall localState appInput appOutput
   . Input localState appInput appOutput
  -> State localState appInput appOutput
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
  , emitterSub: Nothing
  , listener: Nothing
  , interactions
  , queuedUpdates: List.Nil
  , processingUpdates: List.Nil
  , rafId: Nothing
  }

-- | Canvas component's render function. Size/position CSS comes from
-- | `Application`, event properties come from `Interactions`, and
-- | `width`/`height` attributes come from `Dimensions`.
-- |
-- | The `width` and `height` attributes may be different from the size CSS. The
-- | CSS controls the area that the element takes up on the page, while the HTML
-- | attributes control the coordinate system of the drawing area.
render
  :: forall localState appInput appOutput slots m
   . State localState appInput appOutput
  -> H.ComponentHTML (Action localState) slots m
render { name, clientRect, app, interactions } =
  canvas
    $ [ id name, style $ App.windowCss app.window, tabIndex 0 ]
      <> GI.toProps QueueUpdate interactions
      <> maybe [] Dims.toSizeProps clientRect
  where
  style :: forall r i. CSS.CSS -> IProp (style :: String | r) i
  style =
    attr (AttrName "style")
      <<< fromMaybe ""
      <<< CSS.renderedInline
      <<< CSS.rules []
      <<< CSS.runS

-- | - `Initialize`: Create `context`, `resizeSub`, `emitterSub`, `listener`,
-- |   `clientRect`, `canvas`, and `scaler` values. Then recurse with `Tick` to
-- |   request the first animation frame.
-- | - `HandleResize`: Window resized, get new client rect and recalculate
-- |   `scaler` functions.
-- | - `Tick`: Request an animation frame. When animating, `Tick` passes the
-- |   animation frame timestamp to itself so it can calculate the delta between
-- |   frames.
-- | - `Finalize`: Unsubscribe from window resize events and listener/emitter.
-- | - `QueueUpdate`: An event (interaction or input) fired, add the handler to
-- |   a queue to be run on the next animation frame.
-- | - `UpdatesProcessed`: The `processingUpdates` queue was successfully
-- |   processed in an animation frame, so those updates can be discarded.
-- | - `StateUpdated`: The local state is changing. Save it and tell
-- |   `Application` to handle output.
-- | - `FrameRequested`: An animation frame has been requested, save its ID.
-- | - `FrameFired`: The requested animation frame has fired, forget its ID.
handleAction
  :: forall localState appInput appOutput slots m
   . MonadAff m
  => Action localState
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots (Output appOutput) m Unit
handleAction = case _ of
  Initialize -> do
    initialize
    handleAction $ Tick Nothing

  HandleResize -> updateClientRect

  Tick mLastTime -> do
    { listener, context, localState, app, scaler, queuedUpdates, processingUpdates } <- H.get
    -- Prepend any newly queued updates to the list of updates we've tried to
    --   process, then empty the main queue and replace the processing queue
    --   with the sum of both.
    let
      tryUpdates = queuedUpdates <> processingUpdates
    H.modify_
      ( _
          { queuedUpdates = List.Nil
          , processingUpdates = tryUpdates
          }
      )
    queueAnimationFrame mLastTime listener context scaler tryUpdates localState app

  Finalize -> unsubscribe

  -- Hold on to interactions/inputs until the next tick, then pass them into rAF
  QueueUpdate handlerFn -> do
    queuedUpdates <- H.gets _.queuedUpdates
    H.modify_ (_ { queuedUpdates = handlerFn : queuedUpdates })

  -- The interactions passed into an animation frame have been processed and are
  --   no longer needed.
  UpdatesProcessed -> H.modify_ (_ { processingUpdates = List.Nil })

  StateUpdated delta scaler localState' -> saveNewState delta scaler localState'

  FrameRequested rafId -> H.modify_ (_ { rafId = Just rafId })

  FrameFired -> H.modify_ (_ { rafId = Nothing })

-- | Subscribe to window resize events. Get the `canvas` element and its
-- | `Context2D` and `clientRect`. Create scaling functions based on the
-- | `viewBox` and `clientRect`.
initialize
  :: forall localState appInput appOutput slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) (Action localState) slots output m Unit
initialize = do
  resizeSub <- subscribeResize
  { emitter, listener } <- H.liftEffect HS.create
  emitterSub <- H.subscribe emitter
  { name, viewBox } <- H.get
  mcontext <- H.liftEffect $ getContext name
  mcanvas <- H.liftEffect $ getCanvasElement name
  clientRect <- H.liftEffect $ getCanvasClientRect mcanvas
  H.modify_
    ( _
        { context = mcontext
        , resizeSub = Just resizeSub
        , emitterSub = Just emitterSub
        , listener = Just listener
        , clientRect = clientRect
        , canvas = mcanvas
        , scaler = Dims.mkScaler viewBox <$> clientRect
        }
    )

-- | Runs update and render functions:
-- |
-- | First, get the window reference to call `requestAnimationFrame` with
-- | `rafCallback`. `rafCallback` is a closure so that it has access to the
-- | values passed in from the component state. `requestAnimationFrame` returns
-- | an ID which we send to the component via a listener and the
-- | `FrameRequested` action.
-- |
-- | `rafCallback` emits a `FrameFired` action indicating the callback started.
-- | Next, it calculates the time delta based on the current and previous
-- | timestamps. If there is no previous timestamp, the `updateAndRender` call
-- | is skipped and another frame requested.
-- |
-- | The `updateAndRender` call could also be skipped because the client rect
-- | hasn't been found, so there is no scaler, or the canvas element hasn't
-- | been found, so there is no context, or the listener and emitter haven't
-- | been created.
-- |
-- | `updateAndRender` takes the list of queued update handlers and adds the
-- | component's update function. It folds over the list, calling each update
-- | function, and tracking whether any of them changes the state. If they do, a
-- | `StateUpdated` Action is emitted, which will persist the change back to the
-- | component and cause the component to check whether the change needs to be
-- | outputted. Next, an action is emitted to let the component know that the
-- | updates in the `processingUpdates` queue are complete. Finally, it calls
-- | the app's render function.
-- |
-- | After `updateAndRender`, a `Tick` is emitted to request the next frame.
queueAnimationFrame
  :: forall localState appInput appOutput slots output m
   . MonadAff m
  => Maybe (T.TimestampPrevious)
  -> Maybe (HS.Listener (Action localState))
  -> Maybe Context2D
  -> Maybe Dims.Scaler
  -> List (App.Update localState)
  -> localState
  -> App.AppSpec Context2D localState appInput appOutput
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots output m Unit
queueAnimationFrame mLastTime mlistener mcontext mscaler queuedUpdates localState app = do
  H.liftEffect do
    wnd <- window
    rafId <- T.requestAnimationFrame rafCallback wnd
    notify $ FrameRequested rafId
  where
  rafCallback :: T.TimestampCurrent -> Effect Unit
  rafCallback timestamp = do
    notify FrameFired
    mdelta <- pure $ T.delta timestamp <$> mLastTime
    _ <-
      sequence $ updateAndRender
        <$> mlistener
        <*> mdelta
        <*> mcontext
        <*> mscaler
    notify $ Tick $ Just $ T.toPrev timestamp

  notify :: Action localState -> Effect Unit
  notify = case mlistener of
    Nothing -> const $ pure unit
    Just listener -> HS.notify listener

  updateAndRender
    :: HS.Listener (Action localState)
    -> T.Delta
    -> Context2D
    -> Dims.Scaler
    -> Effect Unit
  updateAndRender listener delta context scaler = do
    changed /\ state' <-
      foldr
        (applyUpdate delta scaler)
        (pure (DidNotChange /\ localState))
        (app.update : queuedUpdates)

    case changed of
      Changed -> HS.notify listener $ StateUpdated delta scaler state'
      DidNotChange -> pure unit

    HS.notify listener UpdatesProcessed
    app.render state' delta scaler context

  applyUpdate
    :: T.Delta
    -> Dims.Scaler
    -> App.Update localState
    -> Effect (Tuple StateChanged localState)
    -> Effect (Tuple StateChanged localState)
  applyUpdate delta scaler update s = do
    (_ /\ state) <- s

    mstate' <- App.runUpdate delta scaler state update

    case mstate' of
      Just state' -> pure $ Changed /\ state'
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
getCanvasElement :: String -> Effect (Maybe Element)
getCanvasElement name = do
  doc <- document =<< window
  mcanvas <- getElementById name $ toNonElementParentNode doc
  pure mcanvas

-- | Attempt to get the bounding client rect for an HTML element and convert it
-- | to a `ClientRect` value.
getCanvasClientRect :: Maybe Element -> Effect (Maybe Dims.ClientRect)
getCanvasClientRect mcanvas = do
  (mbounding :: Maybe DOMRect) <- traverse getBoundingClientRect mcanvas
  pure $ Dims.fromDOMRect <$> mbounding

-- | Get a new `clientRect` for the `canvas` element and create a new scaler for
-- | it, saving both to the component state.
updateClientRect
  :: forall localState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) action slots output m Unit
updateClientRect = do
  { canvas, viewBox } <- H.get
  clientRect <- H.liftEffect $ getCanvasClientRect canvas
  H.modify_
    ( _
        { clientRect = clientRect
        , scaler = Dims.mkScaler viewBox <$> clientRect
        }
    )

-- | Unsubscribe from window resize events and paired listener/emitter.
unsubscribe
  :: forall localState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) action slots output m Unit
unsubscribe = do
  mresizeSub <- H.gets _.resizeSub
  traverse_ H.unsubscribe mresizeSub
  memitterSub <- H.gets _.emitterSub
  traverse_ H.unsubscribe memitterSub

-- | Subscribe to window resize events and fire the `HandleResize` `Action` when
-- | they occur.
subscribeResize
  :: forall localState appInput appOutput slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) (Action localState) slots output m H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ HE.eventListener
      (EventType "resize")
      (toEventTarget wnd)
      (const $ Just HandleResize)

-- | Save the updated local state of the application. Compare the old and new
-- | states in the `OutputProducer` function and send output, if necessary.
saveNewState
  :: forall localState appInput appOutput slots m
   . MonadAff m
  => T.Delta
  -> Dims.Scaler
  -> localState
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots (Output appOutput) m Unit
saveNewState delta scaler state' = do
  { app, localState } <- H.get
  H.modify_ (_ { localState = state' })
  traverse_ (H.raise <<< Output) $ app.output delta scaler localState state'

-- | Receiving input from the host application. Convert it into an `Update` and
-- | call `handleAction` to add it to the update queue.
handleQuery
  :: forall localState appInput appOutput slots a m
   . MonadAff m
  => Query appInput a
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots (Output appOutput) m (Maybe a)
handleQuery (Input inData a) = do
  { input } <- H.gets _.app
  handleAction $ QueueUpdate $ App.pureUpdate $ input inData
  pure (Just a)
