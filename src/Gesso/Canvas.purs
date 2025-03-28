-- | Gesso Canvas is a Halogen component that handles creating a canvas element,
-- | calling `requestAnimationFrame`, attaching events, and running render and
-- | update functions.
module Gesso.Canvas
  ( CanvasInput(..)
  , CanvasOutput(..)
  , Slot
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
import Gesso.Geometry (Rect) as Geo
import Gesso.Geometry.Internal (Scalers, mkScalers) as Geo
import Gesso.Interactions.Internal as GI
import Gesso.State (Compare, History)
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
import Web.HTML.HTMLDocument (toEventTarget, visibilityState) as Document
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState(..)) as Document
import Web.HTML.Window (document)
import Web.HTML.Window (toEventTarget) as Window

-- | A Halogen slot type for the Canvas component, which is used to include it
-- | inside another Halogen component.
type Slot input output slot =
  H.Slot (CanvasInput input) (CanvasOutput output) slot

-- | A proxy type for Canvas for use with `Slot`.
_gessoCanvas = Proxy :: Proxy "gessoCanvas"

-- | The internal state of the Canvas component
-- |
-- | - `name` is the name of the application, which doubles as the HTML `id` for
-- |   the canvas element.
-- | - `localState` is the state of the application.
-- | - `viewBox` is the position and dimensions of the drawing area.
-- | - `window` defines how the screen element should size and position itself.
-- | - `behavior` contains functions that make the application do things.
-- |   - `render` draws on the component every animation frame.
-- |   - `update` runs on each animation frame, just before `render`.
-- |   - `fixed` runs at a set interval of time.
-- |   - `interactions` are events attached to the canvas element.
-- |   - `output` defines how (or if) the component should send information out
-- |     to the host application.
-- |   - `input` defines how the component's state should change in response to
-- |     receiving input from the host application.
-- | - `dom`: DOM-related fields available after initialization:
-- |   - `clientRect` is the actual position and dimensions of the canvas
-- |     element.
-- |   - `canvas` is the canvas element.
-- |   - `context` is the `Context2D` for the canvas element.
-- |   - `scalers` is a record containing scaling information for transforming
-- |     coordinates between the drawing and the canvas.
-- | - `subscriptions`: Event subscriptions created during initialization and
-- |   kept until the application is destroyed.
-- |   - `resize` is a subscription to window resize events, to re-check the
-- |     `clientRect` and recreate the `scaler`.
-- |   - `visibility` is a subscription to document `visibilitychange` events,
-- |     to pause or resume running timers.
-- |   - `emitter` is a subscription to a listener/emitter pair used to send
-- |     Actions from `requestAnimationFrame` callbacks to the component.
-- | - `timers` contains two timestamps, which are both set to a default value
-- |   when the component is initialized:
-- |   - `frame` is the timestamp of the most recently fired animation frame.
-- |   - `fixed` is used for accurately spacing fixed-rate updates and it's
-- |     updated after every batch of updates.
-- | - `pendingUpdates` is a list of interactions and Query inputs waiting to be
-- |   applied.
-- | - `rafId` is the ID of the most recently requested animation frame. It's
-- |   set when `requestAnimationFrame` is called and cleared when the animation
-- |   frame callback runs.
type State state input output =
  { name :: String
  , localState :: state
  , viewBox :: Geo.Rect
  , window :: App.WindowMode
  , behavior ::
      { render :: App.RenderFunction state
      , update :: App.UpdateFunction state
      , fixed :: App.FixedUpdate state
      , interactions :: GI.Interactions state
      , output :: App.OutputProducer state output
      , input :: App.InputReceiver state input
      }
  , dom ::
      Maybe
        { clientRect :: Geo.Rect
        , canvas :: GEl.Canvas
        , context :: Context2D
        , scalers :: Geo.Scalers
        }
  , subscriptions ::
      Maybe
        { resize :: H.SubscriptionId
        , visibility :: H.SubscriptionId
        , emitter :: H.SubscriptionId
        }
  , timers ::
      Maybe
        { frame :: T.Last
        , fixed :: T.Last
        }
  , pendingUpdates :: List (T.Stamped (App.TimestampedUpdate state))
  , rafId :: Maybe T.RequestAnimationFrameId
  }

-- | See `handleAction`
data Action state
  = Initialize
  | HandleResize
  | HandleVisibilityChange
  | FirstTick (Action state -> Effect Unit)
  | Tick (Action state -> Effect Unit) T.Last
  | Finalize
  | StateUpdated T.Delta Geo.Scalers (Compare state)
  | QueueUpdate (App.UpdateFunction state)
  | FrameRequested T.RequestAnimationFrameId
  | FrameFired

-- | Used to wrap Output to a parent Halogen component. The component's output
-- | type is defined by the `OutputProducer` in the
-- | [`Gesso.Application.AppSpec`](Gesso.Application.html#t:AppSpec).
newtype CanvasOutput ouput = CanvasOutput ouput

-- | Used to wrap Queries from a parent Halogen component. The component's input
-- | type is defined by the `InputReceiver` in the
-- | [`Gesso.Application.AppSpec`](Gesso.Application.html#t:AppSpec).
data CanvasInput input a = CanvasInput input a

-- | Definition of the Canvas component. Can be used to slot the canvas into a
-- | parent Halogen component.
-- `render` is memoized so that it only re-renders when the dimensions of the
-- canvas element change.
component
  :: forall state input output m
   . MonadAff m
  => H.Component
       (CanvasInput input)
       (App.AppSpec state input output)
       (CanvasOutput output)
       m
component =
  H.mkComponent
    { initialState
    , render: HH.memoized (eq `on` (_.dom >>> map _.clientRect)) renderComponent
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
-- | the update queues, which start empty.
initialState
  :: forall state input output
   . App.AppSpec state input output
  -> State state input output
initialState { name, window, initialState: localState, viewBox, behavior } =
  { name
  , localState
  , viewBox
  , window
  , behavior
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
renderComponent
  :: forall state input output slots m
   . State state input output
  -> H.ComponentHTML (Action state) slots m
renderComponent { name, dom, window, behavior: { interactions } } =
  HH.canvas $ [ id name, GEl.style window, tabIndex 0 ]
    <> GI.toProps QueueUpdate interactions
    <> maybe [] GEl.toSizeProps (dom <#> _.clientRect)

-- | - `Initialize`: Create `subscriptions` and `dom` records, then recurse with
-- |   `FirstTick` to request the first animation frame.
-- | - `HandleResize`: Window resized, get new client rect and recalculate
-- |   `scaler` functions.
-- | - (TODO) `HandleVisibilityChange`: Window visibility has changed; pause or
-- |   resume running timers.
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
  :: forall state input output slots m
   . MonadAff m
  => Action state
  -> H.HalogenM (State state input output) (Action state) slots
       (CanvasOutput output)
       m
       Unit
handleAction = case _ of
  Initialize -> initialize >>= (FirstTick >>> handleAction)

  HandleResize -> updateClientRect

  HandleVisibilityChange -> {- TODO -} 
    H.liftEffect do
      window >>= document >>= Document.visibilityState
        >>= case _ of
          Document.Visible -> pure unit
          Document.Hidden -> pure unit

  FirstTick notify -> H.liftEffect $ getFirstFrame notify

  Tick notify lastFrame -> do
    { localState, behavior: { fixed, update, render }, pendingUpdates } <- H.get

    results <- runMaybeT do
      timers <- MaybeT $ H.gets _.timers
      { context, scalers } <- MaybeT $ H.gets _.dom

      lift $ H.liftEffect do
        -- schedule fixed updates
        let { function, interval } = fixed
        { last, items } <- T.stampInterval timers.fixed function interval
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
            (tryUpdate scalers)
            (pure initialHistory)
            (updateQueue <#> _.item)

        queueAnimationFrame
          lastFrame
          (T.toRatio last interval)
          context
          scalers
          stateHistory
          update
          render
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

  StateUpdated delta scalers stateVersions ->
    saveNewState delta scalers stateVersions

  FrameRequested rafId -> H.modify_ (_ { rafId = Just rafId })

  FrameFired -> H.modify_ (_ { rafId = Nothing })

-- | Subscribe to window resize events. Get the `canvas` element and its
-- | `Context2D` and `clientRect`. Create scaling functions based on the
-- | `viewBox` and `clientRect`.
initialize
  :: forall state input output slots o m
   . MonadAff m
  => H.HalogenM (State state input output) (Action state) slots o m
       (Action state -> Effect Unit)
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
    visibility <- subscribeVisibility
    pure
      { notify: HS.notify notifications.listener
      , subscriptions: Just { resize, visibility, emitter }
      }

  mkDom { name, viewBox } = do
    context <- GEl.getContextByAppName name
    canvas <- GEl.getCanvasByAppName name
    clientRect <- traverse GEl.getCanvasClientRect canvas
    let scalers = Geo.mkScalers viewBox <$> clientRect
    pure $
      { clientRect: _, canvas: _, context: _, scalers: _ }
        <$> clientRect
        <*> canvas
        <*> context
        <*> scalers

-- | The reusable chunk of requesting an animation frame:
-- |
-- | 1. Request the frame and tell the component that the frame was requested.
-- | 2. When the frame fires, notify the component that the frame has fired and
-- |    then call the provided callback function.
-- | 3. After running the callback, tell the component to start the next Tick.
requestAnimationFrame
  :: forall state
   . (T.Now -> Effect Unit)
  -> (Action state -> Effect Unit)
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
  :: forall state
   . (Action state -> Effect Unit)
  -> Effect Unit
getFirstFrame = requestAnimationFrame (const $ pure unit)

-- | Run per-frame update function and render function. Give `render` the newest
-- | state and the state prior to the most recent update, as well as the time
-- | difference between the two. Send the most recent state and the state as of
-- | the beginning of this tick to the app's output function to determine
-- | whether to send I/O.
queueAnimationFrame
  :: forall state
   . T.Last
  -> (T.Now -> Number)
  -> Context2D
  -> Geo.Scalers
  -> History state
  -> App.UpdateFunction state
  -> App.RenderFunction state
  -> (Action state -> Effect Unit)
  -> Effect Unit
queueAnimationFrame
  lastTime
  toIntervalRatio
  context
  scalers
  stateHistory
  update
  render
  notify =
  requestAnimationFrame rafCallback notify
  where
  rafCallback :: T.Now -> Effect Unit
  rafCallback timestamp = do
    let delta = T.delta timestamp lastTime

    history <- tryUpdate scalers (update delta) (pure stateHistory)

    when history.changed
      $ notify
      $ StateUpdated delta scalers { old: history.original, new: history.new }

    render context delta scalers
      { previous: history.old
      , current: history.new
      , t: toIntervalRatio timestamp
      }

-- | Run an update function, using a current state if available, or an older one
-- | if not. When folding over a list of update functions, this makes it easier
-- | to track whether the state has changed while also continuing to pass on the
-- | most current state.
tryUpdate
  :: forall state
   . Geo.Scalers
  -> (Geo.Scalers -> state -> Effect (Maybe state))
  -> Effect (History state)
  -> Effect (History state)
tryUpdate scalers update state = do
  { original, new } <- state
  update scalers new >>= case _ of
    Nothing -> state
    Just new' -> pure { original, old: new, new: new', changed: true }

-- | Get a new `clientRect` for the `canvas` element and create new scalers for
-- | it, saving both to the component state.
updateClientRect
  :: forall state input output action slots o m
   . MonadAff m
  => H.HalogenM (State state input output) action slots o m Unit
updateClientRect = do
  dom' <- H.liftEffect <<< updateDom =<< H.get
  H.modify_ (_ { dom = dom' })
  where
  updateDom { viewBox, dom } = for dom \d -> do
    clientRect <- GEl.getCanvasClientRect d.canvas
    pure d
      { clientRect = clientRect
      , scalers = Geo.mkScalers viewBox clientRect
      }

-- | Unsubscribe from window resize events and paired listener/emitter.
unsubscribe
  :: forall state input output action slots o m
   . MonadAff m
  => H.HalogenM (State state input output) action slots o m Unit
unsubscribe =
  H.gets _.subscriptions
    >>= traverse_ \subs -> do
      H.unsubscribe subs.resize
      H.unsubscribe subs.emitter

-- | Subscribe to window resize events and fire the `HandleResize` `Action` when
-- | they occur.
subscribeResize
  :: forall state input output slots o m
   . MonadAff m
  => H.HalogenM (State state input output) (Action state) slots o m
       H.SubscriptionId
subscribeResize = do
  wnd <- H.liftEffect window
  H.subscribe
    $ HE.eventListener
        (EventType "resize")
        (Window.toEventTarget wnd)
        (const $ Just HandleResize)

-- | Subscribe to document visibility events and fire the
-- | `HandleVisibilityChange` `Action` when they occur.
subscribeVisibility
  :: forall state input output slots o m
   . MonadAff m
  => H.HalogenM (State state input output) (Action state) slots o m
       H.SubscriptionId
subscribeVisibility = do
  doc <- H.liftEffect $ document =<< window
  H.subscribe
    $ HE.eventListener
        (EventType "visibilitychange")
        (Document.toEventTarget doc)
        (const $ Just HandleVisibilityChange)

-- | Save the updated local state of the application. Compare the old and new
-- | states in the `OutputProducer` function and send output, if necessary.
saveNewState
  :: forall state input output slots m
   . MonadAff m
  => T.Delta
  -> Geo.Scalers
  -> Compare state
  -> H.HalogenM (State state input output) (Action state) slots
       (CanvasOutput output)
       m
       Unit
saveNewState delta scalers stateVersions = do
  { output } <- H.gets _.behavior
  H.modify_ (_ { localState = stateVersions.new })
  mOutput <- liftEffect $ output delta scalers stateVersions
  traverse_ (H.raise <<< CanvasOutput) mOutput

-- | Receiving input from the host application. Convert it into an `Update` and
-- | call `handleAction` to add it to the update queue.
handleQuery
  :: forall state input output slots a m
   . MonadAff m
  => CanvasInput input a
  -> H.HalogenM
       (State state input output)
       (Action state)
       slots
       (CanvasOutput output)
       m
       (Maybe a)
handleQuery (CanvasInput inData a) = do
  { input } <- H.gets _.behavior
  handleAction $ QueueUpdate $ input inData
  pure (Just a)
