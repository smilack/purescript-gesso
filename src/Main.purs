module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Gesso.Canvas as Gesso.Canvas
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI Gesso.Canvas.component init body

init :: Gesso.Canvas.Input {}
init = Gesso.Canvas.Input $
  { origin: Gesso.Canvas.Origin { x: 0.0, y: 0.0 }
  , dimensions: Gesso.Canvas.WH { width: 300.0, height: 300.0 }
  , renderFn
  , externalState: {}
  }

renderFn :: forall state. Gesso.Canvas.RenderStyle state
renderFn = Gesso.Canvas.Continuous render
  where
  render :: Number -> state -> Canvas.Context2D -> Effect Unit
  render _ _ context = do
    Canvas.setFillStyle context "red"
    Canvas.fillRect context { x: 100.0, y: 50.0, width: 200.0, height: 25.0 }
