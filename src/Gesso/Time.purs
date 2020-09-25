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

requestAnimationFrame :: (TimestampCurrent -> Effect Unit) -> Window -> Effect RequestAnimationFrameId
requestAnimationFrame fn = map wrap <<< _requestAnimationFrame (mkEffectFn1 $ fn <<< wrap)

newtype Timestamp a
  = Timestamp Number

derive instance newtypeTimestamp :: Newtype (Timestamp a) _

data Now

data Prev

type TimestampCurrent
  = Timestamp Now

type TimestampPrevious
  = Timestamp Prev

toPrev :: TimestampCurrent -> TimestampPrevious
toPrev = wrap <<< unwrap

type Delta
  = { now :: TimestampCurrent, prev :: TimestampPrevious, delta :: Number }

delta :: TimestampCurrent -> TimestampPrevious -> Delta
delta now@(Timestamp n) prev@(Timestamp p) = { prev, now, delta: n - p }
