-- | Timestamps, time deltas, and `requestAnimationFrame`
module Gesso.Time
  ( requestAnimationFrame
  , module Web.HTML.Window
  , TimestampCurrent
  , TimestampPrevious
  , Timestamp
  , Now
  , Prev
  , toPrev
  , Delta
  , delta
  ) where

import Prelude (Unit, map, (<<<), (-), ($))
import Data.Newtype (class Newtype, wrap, unwrap)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Web.HTML (Window)
import Web.HTML.Window (RequestAnimationFrameId, cancelAnimationFrame)

foreign import _requestAnimationFrame :: EffectFn1 Number Unit -> Window -> Effect Int

-- | An interface to JavaScript's `window.requestAnimationFrame` which provides
-- | the timestamp argument to the callback function. `requestAnimationFrame` in
-- | `Web.HTML.Window` only accepts and `Effect Unit` instead of a function.
requestAnimationFrame :: (TimestampCurrent -> Effect Unit) -> Window -> Effect RequestAnimationFrameId
requestAnimationFrame fn = map wrap <<< _requestAnimationFrame (mkEffectFn1 $ fn <<< wrap)

-- | A number representing a specific time in milliseconds. See
-- | [`DOMHighResTimeStamp`](https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp)
newtype Timestamp a
  = Timestamp Number

derive instance newtypeTimestamp :: Newtype (Timestamp a) _

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
toPrev = wrap <<< unwrap

-- | A record containing a current timestamp, the previous timestamp, and the
-- | difference between them. All values are in milliseconds.
type Delta
  = { now :: TimestampCurrent, prev :: TimestampPrevious, delta :: Number }

-- | Create a Delta record from a current and a previous timestamp.
delta :: TimestampCurrent -> TimestampPrevious -> Delta
delta now@(Timestamp n) prev@(Timestamp p) = { prev, now, delta: n - p }
