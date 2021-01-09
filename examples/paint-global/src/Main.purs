module Example.PaintGlobal.Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Gesso (hoist, mkPlainEnv) as G
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Example.PaintGlobal.Root as Root

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    env <- liftEffect $ G.mkPlainEnv $ Root.initialState unit
    runUI (G.hoist env Root.component) unit body
