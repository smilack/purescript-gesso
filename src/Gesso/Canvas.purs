-- | Gesso Canvas is a Halogen component that handles creating a canvas element,
-- | calling requestAnimationFrame, attaching events, and running render and
-- | update functions.
module Gesso.Canvas
  ( Input
  , Query(..)
  , Slot
  , Output(..)
  , _gessoCanvas
  , component
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Foldable (foldr, for_, traverse_)
import Data.Function (on)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as App
import Gesso.Canvas.Element as GEl
import Gesso.Dimensions as Dims
import Gesso.Interactions as GI
import Gesso.Util.Lerp (Versions, History)
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
-- | - `pendingUpdates` is a list of interactions and Query inputs waiting to be
-- |   applied.
-- | - `timers` contains two timestamps, which are both set to a default value
-- |   when the component is initialized:
-- |   - `frame` is the timestamp of the most recently fired animation frame.
-- |   - `fixed` is used for accurately spacing fixed-rate updates and it's
-- |     updated after every batch of updates.
-- | - `rafId` is the ID of the most recently requested animation frame. It's
-- |   set when `requestAnimationFrame` is called and cleared when the animation
-- |   frame callback runs.
type State localState appInput appOutput =
  { name :: String
  , app :: App.AppSpec Context2D localState appInput appOutput
  , localState :: localState
  , viewBox :: Dims.ViewBox
  , interactions :: GI.Interactions localState
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
  , timers ::
      Maybe
        { frame :: T.Last
        , fixed :: T.Last
        }
  , pendingUpdates :: List (T.Stamped (App.TimestampedUpdate localState))
  , rafId :: Maybe T.RequestAnimationFrameId
  }

-- | See [`handleAction`](#v:handleAction)
data Action localState
  = Initialize
  | HandleResize
  | FirstTick (Action localState -> Effect Unit)
  | Tick (Action localState -> Effect Unit) T.Last
  | Finalize
  | StateUpdated T.Delta Dims.Scaler (Versions localState)
  | QueueUpdate (App.UpdateFunction localState)
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
-- |   the canvas element.
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
  , interactions :: GI.Interactions localState
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
  , timers: Nothing
  , pendingUpdates: List.Nil
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

  Tick notify lastFrame -> do
    { localState, app, pendingUpdates } <- H.get

    results <- runMaybeT do
      { fixed } <- MaybeT $ H.gets _.timers
      { context, scaler } <- MaybeT $ H.gets _.dom

      lift $ H.liftEffect do
        -- schedule fixed updates
        let { function, interval } = app.fixed
        { last, items } <- T.stampInterval fixed function interval
        let updateQueue = T.sort (items <> pendingUpdates)

        -- run pending + queued updates
        let
          initialHistory =
            { original: localState
            , old: localState
            , new: localState
            , changed: false
            }
        stateHistory <-
          foldr
            (tryUpdate scaler)
            (pure initialHistory)
            (updateQueue <#> _.item)

        queueAnimationFrame
          lastFrame
          (T.toRatio last interval)
          context
          scaler
          stateHistory
          app
          notify

        pure
          { queue': List.Nil
          , timers': Just { frame: lastFrame, fixed: last }
          }

    case results of
      Nothing -> handleAction Finalize
      Just { queue', timers' } ->
        H.modify_ (_ { pendingUpdates = queue', timers = timers' })

  Finalize -> unsubscribe

  -- Hold on to interactions/inputs until the next tick, then pass them into rAF
  QueueUpdate handlerFn -> do
    { pendingUpdates, timers } <- H.get
    for_ timers \{ frame } -> do
      stampedUpdate <- H.liftEffect $ T.stamp frame handlerFn
      H.modify_ (_ { pendingUpdates = stampedUpdate : pendingUpdates })

  StateUpdated delta scaler stateVersions ->
    saveNewState delta scaler stateVersions

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
  timers <- H.liftEffect mkTimers
  state <- H.get
  dom <- H.liftEffect $ mkDom state
  H.put $ state { dom = dom, subscriptions = subscriptions, timers = timers }
  pure notify
  where
  mkTimers = T.started <#> \t -> Just { frame: t, fixed: t }

  mkSubs = do
    notifications <- H.liftEffect HS.create
    emitter <- H.subscribe notifications.emitter
    resize <- subscribeResize
    pure
      { notify: HS.notify notifications.listener
      , subscriptions: Just { resize, emitter }
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

-- | Run per-frame update function and render function. Give `render` the newest
-- | state and the state prior to the most recent update, as well as the time
-- | difference between the two. Send the most recent state and the state as of
-- | the beginning of this tick to the app's output function to determine
-- | whether to send I/O.
queueAnimationFrame
  :: forall localState appInput appOutput
   . T.Last
  -> (T.Now -> Number)
  -> Context2D
  -> Dims.Scaler
  -> History localState
  -> App.AppSpec Context2D localState appInput appOutput
  -> (Action localState -> Effect Unit)
  -> Effect Unit
queueAnimationFrame lastTime toIntRatio context scaler stateHistory app notify =
  requestAnimationFrame rafCallback notify
  where
  rafCallback :: T.Now -> Effect Unit
  rafCallback timestamp = do
    let delta = T.delta timestamp lastTime

    history <- tryUpdate scaler (app.update delta) (pure stateHistory)

    when stateHistory.changed
      $ notify
      $ StateUpdated delta scaler { old: history.original, new: history.new }

    app.render context delta scaler
      { old: history.old, new: history.new, t: toIntRatio timestamp }

-- | Run an update function, using a current state if available, or an older one
-- | if not. When folding over a list of update functions, this makes it easier
-- | to track whether the state has changed while also continuing to pass on the
-- | most current state.
tryUpdate
  :: forall localState
   . Dims.Scaler
  -> (Dims.Scaler -> localState -> Effect (Maybe localState))
  -> Effect (History localState)
  -> Effect (History localState)
tryUpdate scaler update state = do
  { original, new } <- state
  update scaler new >>= case _ of
    Nothing -> state
    Just new' -> pure { original, old: new, new: new', changed: true }

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
  -> Versions localState
  -> H.HalogenM
       (State localState appInput appOutput)
       (Action localState)
       slots
       (Output appOutput)
       m
       Unit
saveNewState delta scaler stateVersions = do
  { app: { output } } <- H.get
  H.modify_ (_ { localState = stateVersions.new })
  mOutput <- liftEffect $ output delta scaler stateVersions
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
