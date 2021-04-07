-- | Timestamps, time deltas, and `requestAnimationFrame`
module Gesso.Time
  ( requestAnimationFrame
  , cancelAnimationFrame
  , RequestAnimationFrameId
  , TimestampCurrent
  , TimestampPrevious
  , Timestamp
  , Now
  , Prev
  , toPrev
  , Delta
  , delta
  ) where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Prelude (Unit, map, (<<<), (-), ($))
import Safe.Coerce (coerce)
import Web.HTML (Window)

newtype RequestAnimationFrameId = RequestAnimationFrameId Int

foreign import cancelAnimationFrame :: RequestAnimationFrameId -> Window -> Effect Unit

foreign import _requestAnimationFrame :: EffectFn1 Number Unit -> Window -> Effect Int

-- | An interface to JavaScript's `window.requestAnimationFrame` which provides
-- | the timestamp argument to the callback function. `requestAnimationFrame` in
-- | `Web.HTML.Window` only accepts and `Effect Unit` instead of a function.
requestAnimationFrame :: (TimestampCurrent -> Effect Unit) -> Window -> Effect RequestAnimationFrameId
requestAnimationFrame fn = map RequestAnimationFrameId <<< _requestAnimationFrame (mkEffectFn1 $ fn <<< Timestamp)

-- | A number representing a specific time in milliseconds. See
-- | [`DOMHighResTimeStamp`](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp)
newtype Timestamp :: forall k. k -> Type
newtype Timestamp a
  = Timestamp Number

-- | A phantom type for tagging Timestamps
data Now

-- | A phantom type for tagging Timestamps
data Prev

-- | Alias for a timestamp representing the time the current frame started
type TimestampCurrent
  = Timestamp Now

-- | Alias for a timestamp representing the time the previous frame started
type TimestampPrevious
  = Timestamp Prev

-- | Mark a current time as the new previous time
toPrev :: TimestampCurrent -> TimestampPrevious
toPrev = coerce

-- | A record containing a current timestamp, the previous timestamp, and the
-- | difference between them. All values are in milliseconds.
type Delta
  = { now :: TimestampCurrent, prev :: TimestampPrevious, delta :: Number }

-- | Create a Delta record from a current and a previous timestamp.
delta :: TimestampCurrent -> TimestampPrevious -> Delta
delta now@(Timestamp n) prev@(Timestamp p) = { prev, now, delta: n - p }
