module Debug (unsafeLogAnything, module Effect.Console) where

import Prelude (Unit)
import Effect (Effect)
import Effect.Console (log, logShow)

foreign import _unsafeLogAnything :: forall a. a -> Effect Unit

unsafeLogAnything :: forall a. a -> Effect Unit
unsafeLogAnything = _unsafeLogAnything
