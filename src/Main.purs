module Main where

import Prelude
import Effect (Effect)
import Gesso.Canvas as Gesso.Canvas
import Gesso.Dimensions as Dims
import Graphics.Canvas as Canvas
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Math (cos, sin)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI Gesso.Canvas.component init body

type AppState
  = { color :: String }

init :: Gesso.Canvas.Input AppState
init =
  Gesso.Canvas.Input
    $ { boundingBox:
          Dims.fromPointAndSize
            Dims.origin
            (Dims.fromWidthAndHeight { width: 300.0, height: 300.0 })
      , renderFn
      , appState: { color: "blue" }
      }

renderFn :: Gesso.Canvas.RenderStyle AppState
renderFn = Gesso.Canvas.Continuous render
  where
  render :: Number -> Number -> AppState -> Canvas.Context2D -> Effect Unit
  render timestamp delta { color } context = do
    Canvas.clearRect context { x: 0.0, y: 0.0, width: 300.0, height: 300.0 }
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: 100.0, y: 50.0, width: 200.0, height: 25.0 }
    let
      t = timestamp * 6.28 / 1000.0 / 2.0
    Canvas.fillRect context { x: 150.0 + 50.0 * cos t, y: 150.0 + 50.0 * sin t, width: 10.0, height: 10.0 }
