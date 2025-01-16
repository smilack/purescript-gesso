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
import Data.Traversable (for, traverse)
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
-- | - `subscriptions`: Event subscriptions created during initialization and
-- |   kept until the application is destroyed.
-- |   - `resize` is a subscription to window resize events, to re-check the
-- |     `clientRect` and recreate the `scaler`.
-- |   - `emitter` is a subscription to a listener/emitter pair used to send
-- |     Actions from `requestAnimationFrame` callbacks to the component.
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
  | FirstTick (Action localState -> Effect Unit)
  | Tick (Action localState -> Effect Unit) T.Last
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

-- | The input provided when the Canvas component is created. Because, of these
-- | fields, the only one that should be changed from outside the component is
-- | `localState`, the component has no `receive` defined in its `EvalSpec` (see
-- | [`component`](#v:component)'s use of `defaultEval`) so that this input is
-- | only read once.
-- |
-- | Instead, for outside changes to `localState`, an `input` function can be
-- | provided in the `AppSpec`, and the `input` function will have access to the
-- | same arguments as a regular update function.
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
-- | re-renders when the dimensions of the canvas element change.
component
  :: forall localState appInput appOutput m
   . MonadAff m
  => H.Component
       (Query appInput)
       (Input localState appInput appOutput)
       (Output appOutput)
       m
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
    <> maybe [] Dims.toSizeProps (dom <#> _.clientRect)

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
  -> H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       (Output appOutput)
       m
       Unit
handleAction = case _ of
  Initialize -> initialize >>= (FirstTick >>> handleAction)

  HandleResize -> updateClientRect

  FirstTick notify -> H.liftEffect $ getFirstFrame notify

  Tick notify lastTime -> H.gets _.dom >>= case _ of
    -- if we get to this point and the dom stuff isn't available, something's
    --   wrong, so just close
    Nothing -> handleAction Finalize

    -- otherwise, update and render
    Just dom -> do
      { localState, app, queuedUpdates, processingUpdates } <- H.get
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
      H.liftEffect $ queueAnimationFrame
        lastTime
        dom.context
        dom.scaler
        tryUpdates
        localState
        app
        notify

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
  => H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       output
       m
       (Action localState -> Effect Unit)
initialize = do
  { notify, subscriptions } <- mkSubs
  dom <- H.liftEffect <<< mkDom =<< H.get
  H.modify_ (_ { dom = dom, subscriptions = Just subscriptions })
  pure notify
  where
  mkSubs = do
    notifications <- H.liftEffect HS.create
    emitter <- H.subscribe notifications.emitter
    resize <- subscribeResize
    pure
      { notify: HS.notify notifications.listener
      , subscriptions: { resize, emitter }
      }

  mkDom { name, viewBox } = do
    context <- GEl.getContextByAppName name
    canvas <- GEl.getCanvasByAppName name
    clientRect <- traverse GEl.getCanvasClientRect canvas
    let scaler = Dims.mkScaler viewBox <$> clientRect
    pure $
      { clientRect: _, canvas: _, context: _, scaler: _ }
        <$> clientRect
        <*> canvas
        <*> context
        <*> scaler

-- | The reusable chunk of requesting an animation frame:
-- |
-- | 1. Request the frame and tell the component that the frame was requested.
-- | 2. When the frame fires, notify the component that the frame has fired and
-- |    then call the provided callback function.
-- | 3. After running the callback, tell the component to start the next Tick.
requestAnimationFrame
  :: forall localState
   . (T.Now -> Effect Unit)
  -> (Action localState -> Effect Unit)
  -> Effect Unit
requestAnimationFrame callback notify =
  window
    >>= T.requestAnimationFrame callbackWrapper
    >>= (FrameRequested >>> notify)
  where
  callbackWrapper timestamp =
    notify FrameFired
      *> callback timestamp
      *> tick timestamp

  tick = notify <<< Tick notify <<< T.elapse

-- | Request one animation frame in order to get a timestamp to start counting
-- | from.
getFirstFrame
  :: forall localState
   . (Action localState -> Effect Unit)
  -> Effect Unit
getFirstFrame = requestAnimationFrame (const $ pure unit)

-- | Runs update and render functions:
-- |
-- | `updateAndRender` takes the list of queued update handlers and prepends the
-- | component's update function. It folds over the list, calling each update
-- | function, and tracking whether any of them changes the state. If one does,
-- | a `StateUpdated` action is emitted, which will persist the change back to
-- | the component and cause the component to check whether the change needs to
-- | be outputted. Next, an action is emitted to let the component know that the
-- | updates in the `processingUpdates` queue are complete. Finally, it calls
-- | the app's render function.
queueAnimationFrame
  :: forall localState appInput appOutput
   . T.Last
  -> Context2D
  -> Dims.Scaler
  -> List (App.UpdateFunction localState)
  -> localState
  -> App.AppSpec Context2D localState appInput appOutput
  -> (Action localState -> Effect Unit)
  -> Effect Unit
queueAnimationFrame
  lastTime
  context
  scaler
  queuedUpdates
  localState
  app
  notify =
  requestAnimationFrame rafCallback notify
  where
  rafCallback :: T.Now -> Effect Unit
  rafCallback timestamp = updateAndRender (T.delta timestamp lastTime)

  -- TODO: There's a better way to do this and `applyUpdate`. Writer monad?
  updateAndRender :: T.Delta -> Effect Unit
  updateAndRender delta = do
    changed /\ state' <-
      foldr
        (applyUpdate delta)
        (pure (DidNotChange /\ localState))
        (app.update : queuedUpdates)

    case changed of
      Changed -> notify $ StateUpdated delta scaler state'
      DidNotChange -> pure unit

    notify UpdatesProcessed

    app.render state' delta scaler context

  applyUpdate
    :: T.Delta
    -> App.UpdateFunction localState
    -> Effect (Tuple StateChanged localState)
    -> Effect (Tuple StateChanged localState)
  applyUpdate delta update s = do
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
  dom' <- H.liftEffect <<< updateDom =<< H.get
  H.modify_ (_ { dom = dom' })
  where
  updateDom { viewBox, dom } = for dom \d -> do
    clientRect <- GEl.getCanvasClientRect d.canvas
    pure d
      { clientRect = clientRect
      , scaler = Dims.mkScaler viewBox clientRect
      }

-- | Unsubscribe from window resize events and paired listener/emitter.
unsubscribe
  :: forall localState appInput appOutput action slots output m
   . MonadAff m
  => H.HalogenM (State localState appInput appOutput) action slots output m Unit
unsubscribe =
  H.gets _.subscriptions
    >>= traverse_ \subs -> do
      H.unsubscribe subs.resize
      H.unsubscribe subs.emitter

-- | Subscribe to window resize events and fire the `HandleResize` `Action` when
-- | they occur.
subscribeResize
  :: forall localState appInput appOutput slots output m
   . MonadAff m
  => H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       output
       m
       H.SubscriptionId
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
  -> H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       (Output appOutput)
       m
       Unit
saveNewState delta scaler state' = do
  { app: { output }, localState } <- H.get
  H.modify_ (_ { localState = state' })
  mOutput <- liftEffect $ output delta scaler localState state'
  traverse_ (H.raise <<< Output) mOutput

-- | Receiving input from the host application. Convert it into an `Update` and
-- | call `handleAction` to add it to the update queue.
handleQuery
  :: forall localState appInput appOutput slots a m
   . MonadAff m
  => Query appInput a
  -> H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       (Output appOutput)
       m
       (Maybe a)
handleQuery (Input inData a) = do
  { app: { input } } <- H.get
  handleAction $ QueueUpdate $ input inData
  pure (Just a)
