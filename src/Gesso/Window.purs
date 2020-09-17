module Gesso.Window (requestAnimationFrame, module Web.HTML.Window) where

import Prelude (Unit, map, (<<<))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Web.HTML (Window)
import Web.HTML.Window (RequestAnimationFrameId, cancelAnimationFrame)

foreign import _requestAnimationFrame :: EffectFn1 Number Unit -> Window -> Effect Int

requestAnimationFrame :: (Number -> Effect Unit) -> Window -> Effect RequestAnimationFrameId
requestAnimationFrame fn = map wrap <<< _requestAnimationFrame (mkEffectFn1 fn)
