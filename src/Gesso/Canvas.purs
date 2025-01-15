-- | Gesso Canvas is a Halogen component that handles creating a canvas element,
-- | calling requestAnimationFrame, attaching events, and running render and
-- | update functions.
module Gesso.Canvas
  ( Action
  , Input
  , Query(..)
  , Slot
  , Output(..)
  , _gessoCanvas
  , component
  ) where

import Prelude

import Data.Foldable (foldr, traverse_)
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as App
import Gesso.Canvas.Element as GEl
import Gesso.Dimensions as Dims
import Gesso.Interactions as GI
import Gesso.Time as T
import Graphics.Canvas (Context2D)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (memoized, canvas) as HH
import Halogen.HTML.Properties (id, tabIndex)
import Halogen.Query.Event as HE
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

-- | The Halogen slot type for Canvas, which is used to include it inside
-- | another Halogen component.
type Slot input output slot = H.Slot (Query input) (Output output) slot

-- | A proxy type for Canvas provided for convenience, for use with Slot.
_gessoCanvas = Proxy :: Proxy "gessoCanvas"

-- | The internal state of the Canvas component
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `app` is an `AppSpec`.
-- | - `localState` is the state of the application.
-- | - `viewBox` is the position and dimensions of the drawing area.
-- | - `interactions` is the events attached to the canvas element.
-- | - `dom`: DOM-related fields available after initialization:
-- |   - `clientRect` is the actual position and dimensions of the canvas
-- |     element.
-- |   - `canvas` is the canvas element.
-- |   - `context` is the `Context2D` for the canvas element.
-- |   - `scaler` is the scaler record that converts between viewbox and client
-- |     rect coordinates.
-- |   - `listener` is part of a listener/emitter pair used to send Actions from
-- |     `requestAnimationFrame` callbacks to the component.
-- | - `subscriptions`: Event subscriptions created during initialization and
-- |   kept until the application is destroyed.
-- |   - `resize` is a subscription to window resize events, to re-check the
-- |     `clientRect` and recreate the `scaler`.
-- |   - `emitter` is part of a listener/emitter pair used to send Actions from
-- |     `requestAnimationFrame` callbacks to the component.
-- | - `queuedUpdates` is a list of interactions and Query inputs waiting to be
-- |   applied.
-- | - `processingUpdates` is a list of interactions and Query inputs that have
-- |   been passed to the animation frame to be applied, but which may not have
-- |   been applied yet.
-- | - `rafId` is the ID of the most recently requested animation frame. It's
-- |   set when `requestAnimationFrame` is called and cleared when the animation
-- |   frame callback runs.
type State localState appInput appOutput =
  { name :: String
  , app :: App.AppSpec Context2D localState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , interactions :: GI.Interactions localState (Action localState)
  , dom ::
      Maybe
        { clientRect :: Dims.ClientRect
        , canvas :: GEl.Canvas
        , context :: Context2D
        , scaler :: Dims.Scaler
        , listener :: HS.Listener (Action localState)
        }
  , subscriptions ::
      Maybe
        { resize :: H.SubscriptionId
        , emitter :: H.SubscriptionId
        }
  , queuedUpdates :: List (App.UpdateFunction localState)
  , processingUpdates :: List (App.UpdateFunction localState)
  , rafId :: Maybe T.RequestAnimationFrameId
  }

-- | See [`handleAction`](#v:handleAction)
data Action localState
  = Initialize
  | HandleResize
  | FirstTick
  | Tick T.Last
  | Finalize
  | StateUpdated T.Delta Dims.Scaler localState
  | QueueUpdate (App.UpdateFunction localState)
  | UpdatesProcessed
  | FrameRequested T.RequestAnimationFrameId
  | FrameFired

-- | The component's output type is defined by the `OutputProducer` in the
-- | `Application.AppSpec`.
newtype Output appOutput = Output appOutput

-- | The component's input type is defined by the `InputReceiver` in the
-- | `Application.AppSpec`.
data Query appInput a = Input appInput a

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
type Input localState appInput appOutput =
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
    , render: HH.memoized (eq `on` (_.dom >>> map _.clientRect)) render
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
  , interactions
  , dom: Nothing
  , subscriptions: Nothing
  , queuedUpdates: List.Nil
  , processingUpdates: List.Nil
  , rafId: Nothing
  }

-- | Render Canvas component. The `width` and `height` attributes may be
-- | different from the CSS width and height. The CSS controls the area that the
-- | element takes up on the page, while the HTML attributes control the
-- | coordinate system of the drawing area.
render
  :: forall localState appInput appOutput slots m
   . State localState appInput appOutput
  -> H.ComponentHTML (Action localState) slots m
render { name, dom, app, interactions } =
  HH.canvas $ [ id name, GEl.style app.window, tabIndex 0 ]
    <> GI.toProps QueueUpdate interactions
    <> maybe [] Dims.toSizeProps (_.clientRect <$> dom)

-- | - `Initialize`: Create `subscriptions` and `dom` records, then recurse with
-- |   `FirstTick` to request the first animation frame.
-- | - `HandleResize`: Window resized, get new client rect and recalculate
-- |   `scaler` functions.
-- | - `FirstTick`: Request an animation frame that only checks the time and
-- |   then starts the `Tick` loop, so that `Tick` can start out knowing the
-- |   frame timing.
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
    handleAction FirstTick

  HandleResize -> updateClientRect

  FirstTick -> do
    state <- H.get
    let mlistener = (_.listener) <$> state.dom
    getFirstFrameTime mlistener

  Tick lastTime -> do
    { dom, localState, app, queuedUpdates, processingUpdates } <- H.get
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
    queueAnimationFrame
      lastTime
      (_.listener <$> dom)
      (_.context <$> dom)
      (_.scaler <$> dom)
      tryUpdates
      localState
      app

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
  mcontext <- H.liftEffect $ GEl.getContextByAppName name
  mcanvas <- H.liftEffect $ GEl.getCanvasByAppName name
  clientRect <- H.liftEffect $ traverse GEl.getCanvasClientRect mcanvas
  H.modify_
    ( _
        { dom = { clientRect: _, canvas: _, context: _, scaler: _, listener }
            <$> clientRect
            <*> mcanvas
            <*> mcontext
            <*> (Dims.mkScaler viewBox <$> clientRect)
        , subscriptions = Just { resize: resizeSub, emitter: emitterSub }
        }
    )

-- | Request one animation frame in order to get a timestamp to start from.
getFirstFrameTime
  :: forall componentState localState slots output m
   . MonadAff m
  => Maybe (HS.Listener (Action localState))
  -> H.HalogenM componentState (Action localState) slots output m Unit
getFirstFrameTime listener =
  H.liftEffect do
    wnd <- window
    rafId <- T.requestAnimationFrame firstFrameCallback wnd
    notify (FrameRequested rafId)
  where
  firstFrameCallback :: T.Now -> Effect Unit
  firstFrameCallback timestamp = do
    notify FrameFired
    notify (Tick $ T.elapse timestamp)

  notify :: Action localState -> Effect Unit
  notify action = void $ sequence $ HS.notify <$> listener <@> action

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
  => T.Last
  -> Maybe (HS.Listener (Action localState))
  -> Maybe Context2D
  -> Maybe Dims.Scaler
  -> List (App.UpdateFunction localState)
  -> localState
  -> App.AppSpec Context2D localState appInput appOutput
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots output m Unit
queueAnimationFrame lastTime mlistener mcontext mscaler queuedUpdates localState app = do
  H.liftEffect do
    wnd <- window
    rafId <- T.requestAnimationFrame rafCallback wnd
    notify $ FrameRequested rafId
  where
  rafCallback :: T.Now -> Effect Unit
  rafCallback timestamp = do
    notify FrameFired
    delta <- pure $ T.delta timestamp lastTime
    _ <-
      sequence $ updateAndRender
        <$> mlistener
        <*> Just delta
        <*> mcontext
        <*> mscaler
    notify $ Tick $ T.elapse timestamp

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
    -> App.UpdateFunction localState
    -> Effect (Tuple StateChanged localState)
    -> Effect (Tuple StateChanged localState)
  applyUpdate delta scaler update s = do
    (_ /\ state) <- s

    mstate' <- update delta scaler state

    case mstate' of
      Just state' -> pure $ Changed /\ state'
      Nothing -> s

-- | A simple type to track whether the local state changed while running
-- | interactions and updates.
data StateChanged
  = Changed
  | DidNotChange

-- | Get a new `clientRect` for the `canvas` element and create a new scaler for
-- | it, saving both to the component state.
updateClientRect
  :: forall localState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) action slots output m Unit
updateClientRect = do
  state <- H.get
  d' <- H.liftEffect $ case state.dom of
    Nothing -> pure state.dom
    Just d -> do
      let
        can = d.canvas
        vb = state.viewBox
      cliRec <- GEl.getCanvasClientRect can
      pure $ Just $ d { clientRect = cliRec, scaler = Dims.mkScaler vb cliRec }
  let state' = state { dom = d' }
  H.put state'

-- | Unsubscribe from window resize events and paired listener/emitter.
unsubscribe
  :: forall localState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) action slots output m Unit
unsubscribe = do
  msubs <- H.gets _.subscriptions
  traverse_ H.unsubscribe (_.resize <$> msubs)
  traverse_ H.unsubscribe (_.emitter <$> msubs)

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
  mOutput <- liftEffect $ app.output delta scaler localState state'
  traverse_ (H.raise <<< Output) mOutput

-- | Receiving input from the host application. Convert it into an `Update` and
-- | call `handleAction` to add it to the update queue.
handleQuery
  :: forall localState appInput appOutput slots a m
   . MonadAff m
  => Query appInput a
  -> H.HalogenM (State localState appInput appOutput) (Action localState) slots (Output appOutput) m (Maybe a)
handleQuery (Input inData a) = do
  { input } <- H.gets _.app
  handleAction $ QueueUpdate $ input inData
  pure (Just a)
