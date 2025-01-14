-- | Timestamps, time deltas, and `requestAnimationFrame`
module Gesso.Time
  ( requestAnimationFrame
  , cancelAnimationFrame
  , RequestAnimationFrameId
  , Now
  , Last
  , elapse
  , Delta
  , delta
  , stamp
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Prelude (Unit, (<<<), (-), bind, pure)
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

-- | A time in the past, in milliseconds, representing the last time an
-- | animation frame fired.
newtype Last = Last Number

-- | Convert a current time into a previous time.
elapse :: Now -> Last
elapse (Now t) = Last t

-- | A current time, a previous time, and the difference between them. All
-- | values are in milliseconds.
-- |
-- | Typically, `last` will be the last time an animation frame fired. `now`
-- | will either represent:
-- |
-- | * for per-frame updates and rendering, the current animation frame time, or
-- | * for interactions and input, the time when a function was queued
type Delta = { now :: Number, last :: Number, delta :: Number }

-- | Create a Delta record from a current and a previous timestamp.
delta :: Now -> Last -> Delta
delta (Now now) (Last last) = { now, last, delta: now - last }

-- | Get the current `DOMHighResTimeStamp` from `performance.now`.
-- |
-- | See [`DOMHighResTimeStamp`](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp)
foreign import _now :: Effect Now

-- | An item and a specific time associated with that item. Used for comparing
-- | timestamped values produced by [`stamp`](#v:stamp).
type Stamped a = { time :: Number, item :: a }

-- | For a `Last` timestamp and a function `f :: Delta -> a`, such as:
-- | ```purescript
-- | UpdateFunction s :: Delta -> TimestampedUpdate s
-- | ```
-- | in [`Application`](Gesso.Application.html#t:UpdateFunction), create a
-- | `Delta` using the current time and return the current time and the result
-- | of applying the `Delta` to `f`.
stamp :: forall a. Last -> (Delta -> a) -> Effect (Stamped a)
stamp last f = do
  n@(Now t) <- _now
  let d = delta n last
  pure { time: t, item: f d }
