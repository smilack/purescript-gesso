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
import Prelude (Unit, map, (<<<), (-))
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

-- | Get the current `DOMHighResTimeStamp` from `performance.now`.
foreign import _now :: Effect Now

-- | The current time in milliseconds. See
-- | [`DOMHighResTimeStamp`](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp)
newtype Now = Now Number

-- | A time in the past, in milliseconds, representing the last time an
-- | animation frame fired.
newtype Last = Last Number

-- | Convert a current time into a previous time.
elapse :: Now -> Last
elapse (Now t) = Last t

-- | A record containing a current time, a previous time, and the difference
-- | between them. All values are in milliseconds.
type Delta = { now :: Number, last :: Number, delta :: Number }

-- | Create a Delta record from a current and a previous timestamp.
delta :: Now -> Last -> Delta
delta (Now now) (Last last) = { now, last, delta: now - last }

-- | Create a Delta record using just a previous time and pass it to a function
-- | of `Delta -> anything`. Used to convert an
-- | [`Application.UpdateFunction`](Gesso.Application.html#t:UpdateFunction)
-- | into an
-- | [`Application.TimestampedUpdate`](Gesso.Application.html#t:TimestampedUpdate).
-- | Requires an Effect context in order to check the current time.
stamp :: forall a. Last -> (Delta -> a) -> Effect a
stamp last f = f `map` map (_ `delta` last) _now
