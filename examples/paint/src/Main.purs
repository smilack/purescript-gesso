module Example.Paint.Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Example.Paint.Root as Root

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI Root.component unit body
