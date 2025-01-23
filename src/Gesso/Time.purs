-- | Timestamps, time deltas, and `requestAnimationFrame`
module Gesso.Time
  ( Delta
  , Interval
  , Last
  , Now
  , RequestAnimationFrameId
  , Stamped
  , cancelAnimationFrame
  , delta
  , elapse
  , hz
  , never
  , requestAnimationFrame
  , sort
  , stamp
  , stampInterval
  , started
  ) where

import Prelude

import Data.Function (on)
import Data.List (List(..), head, (:), sortBy)
import Data.Maybe (maybe)
import Data.Number (isFinite)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Web.HTML (Window)

-- | A `RequestAnimationFrameId` is returned when calling
-- | `requestAnimationFrame`. This can be used to cancel the request.
newtype RequestAnimationFrameId = RequestAnimationFrameId Int

-- | Cancel a request for an animation frame using the `RequestAnimationFrameId`
-- | returned by `requestAnimationFrame`.
foreign import cancelAnimationFrame
  :: RequestAnimationFrameId -> Window -> Effect Unit

foreign import _requestAnimationFrame
  :: EffectFn1 Now Unit -> Window -> Effect RequestAnimationFrameId

-- | An interface to `window.requestAnimationFrame` which passes the timestamp
-- | argument to the callback function.
-- |
-- | Provided because `requestAnimationFrame` in `Web.HTML.Window` only accepts
-- | an `Effect Unit` instead of a function of `Number -> Effect Unit`.
requestAnimationFrame
  :: (Now -> Effect Unit)
  -> Window
  -> Effect RequestAnimationFrameId
requestAnimationFrame = _requestAnimationFrame <<< mkEffectFn1

-- | The current time in milliseconds, or the time at which a value became
-- | `Stamped`.
newtype Now = Now Number

-- | A time in the past, in milliseconds.
newtype Last = Last Number

-- | Convert a current time into a previous time.
elapse :: Now -> Last
elapse (Now t) = Last t

-- | A current time, a previous time, and the difference between them. All
-- | values are in milliseconds.
-- |
-- | For per-frame updates and the rendering function, `last` is the time of the
-- | previous animation frame and `now` is the time of the current animation
-- | frame.
-- |
-- | For fixed-rate updates, `last` is the scheduled time of the previous
-- | fixed-rate update and `now` is the update's scheduled time.
-- |
-- | For interaction events and Halogen queries, `last` is the time of the
-- | previous animation frame and `now` is the event's arrival time.
type Delta = { now :: Number, last :: Number, delta :: Number }

-- | Create a Delta from a current time and a previous time.
delta :: Now -> Last -> Delta
delta (Now now) (Last last) = { now, last, delta: now - last }

-- | Get the current `DOMHighResTimeStamp` from `performance.now`.
-- |
-- | See [`DOMHighResTimeStamp`](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp)
foreign import _now :: Effect Now

-- | Get a single `Last` value at the current time, useful for starting a timer.
started :: Effect Last
started = elapse <$> _now

-- | An item and a specific time associated with that item. Used for comparing
-- | timestamped values produced by [`stamp`](#v:stamp) and
-- | [`stampInterval`](#v:stampInterval).
type Stamped a = { time :: Number, item :: a }

sort :: forall a. List (Stamped a) -> List (Stamped a)
sort = sortBy (compare `on` _.time)

-- | For a `last` timestamp and a function `f :: Delta -> a` (such as
-- | `UpdateFunction s :: Delta -> TimestampedUpdate s` in
-- | [`Application`](Gesso.Application.html#t:UpdateFunction)), create a
-- | `Delta` between `last` and now, apply the delta to `f`, and return the
-- | current time and the result of `f delta`.
stamp :: forall a. Last -> (Delta -> a) -> Effect (Stamped a)
stamp last f = do
  n@(Now t) <- _now
  let d = delta n last
  pure { time: t, item: f d }

-- | A interval of time, in milliseconds, to space out repeating an action, or
-- | `Never` perform the action.
data Interval
  = Interval Number
  | Never

-- | Construct an `Interval` from Hz or frames per second. Invalid frequencies
-- | (`±Infinity`, `NaN`, zero, or negative) result in a `Never` interval.
hz :: Number -> Interval
hz fps
  | not (isFinite fps) = Never
  | fps <= 0.0 = Never
  | otherwise = Interval $ 1.0 / fps

-- | An interval that never occurs.
never :: Interval
never = Never

-- | Results of repeatedly timestamping a function at certain interval:
-- |
-- | * `last`: the timestamp of the latest item in the list
-- | * `items`: a list of repeated applications of `Delta`s to the function,
-- |   with timestamps
type StampedBatch a =
  { last :: Last
  , items :: List (Stamped a)
  }

-- | A dummy `StampedBatch` for `Never` intervals. Contains a valid `Last` time
-- | just in case.
emptyBatch :: forall a. Effect (StampedBatch a)
emptyBatch = { last: _, items: Nil } <$> elapse <$> _now

-- | Repeatedly timestamp a function at a given interval, starting at the last
-- | time plus the interval, as long as the timestamp is not after the current
-- | time.
-- |
-- | The `last` value returned should be saved by the caller to pass in as the
-- | `last` argument for the next call to `stampInterval`.
stampInterval
  :: forall a
   . Last
  -> (Delta -> a)
  -> Interval
  -> Effect (StampedBatch a)
stampInterval last fn = case _ of
  Never -> emptyBatch
  Interval ms -> batch <$> schedule Nil last <$> _now
    where
    batch :: List (Stamped a) -> StampedBatch a
    batch items = { last: lastTime items, items }

    lastTime :: List (Stamped a) -> Last
    lastTime l = maybe last (Last <<< _.time) $ head l

    schedule :: List (Stamped a) -> Last -> Now -> List (Stamped a)
    schedule items prev@(Last p) now@(Now n)
      | p + ms >= n = items
      | otherwise = schedule items' (elapse cur) now
          where
          cur@(Now c) = Now $ p + ms
          items' = { time: c, item: fn (delta cur prev) } : items
