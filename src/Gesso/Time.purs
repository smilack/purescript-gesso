module Gesso.Time
  ( requestAnimationFrame
  , module Web.HTML.Window
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

requestAnimationFrame :: (Timestamp Now -> Effect Unit) -> Window -> Effect RequestAnimationFrameId
requestAnimationFrame fn = map wrap <<< _requestAnimationFrame (mkEffectFn1 $ fn <<< wrap)

newtype Timestamp a
  = Timestamp Number

derive instance newtypeTimestamp :: Newtype (Timestamp a) _

data Now

data Prev

toPrev :: Timestamp Now -> Timestamp Prev
toPrev = wrap <<< unwrap

type Delta
  = { now :: Timestamp Now, prev :: Timestamp Prev, delta :: Number }

delta :: Timestamp Now -> Timestamp Prev -> Delta
delta now@(Timestamp n) prev@(Timestamp p) = { prev, now, delta: n - p }
