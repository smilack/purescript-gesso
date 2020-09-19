module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Gesso.Canvas as Gesso.Canvas
import Gesso.Dimensions as Dims
import Gesso.Time as T
import Graphics.Canvas as Canvas
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Math (cos, sin)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    io <- runUI Gesso.Canvas.component init body
    -- io.subscribe - subscribe to stream of output messages
    -- _ <- io.query $ H.tell $ Gesso.Canvas.UpdateAppState {...}
    -- -- io.dispose - kill Halogen app
    pure unit

type AppState
  = { color :: String
    , mouseX :: Maybe Int
    , mouseY :: Maybe Int
    }

initialState :: AppState
initialState = { color: "blue", mouseX: Nothing, mouseY: Nothing }

init :: Gesso.Canvas.Input AppState
init =
  Gesso.Canvas.Input
    $ { boundingBox:
          Dims.fromPointAndSize
            Dims.origin
            (Dims.fromWidthAndHeight { width: 2560.0, height: 1440.0 })
      , renderFn
      , appState: initialState
      }

renderFn :: Gesso.Canvas.RenderStyle AppState
renderFn = Gesso.Canvas.Continuous render
  where
  render :: AppState -> T.Delta -> Canvas.Context2D -> Effect Unit
  render { color, mouseX } { now } context = do
    Canvas.setFillStyle context "#DDFFDD"
    Canvas.fillRect context { x: 0.0, y: 0.0, width: 2560.0, height: 1440.0 }
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: 100.0, y: 50.0, width: 200.0, height: 25.0 }
    let
      t = (unwrap now) * 6.28 / 1000.0 / 2.0
    Canvas.fillRect context { x: 150.0 + 50.0 * cos t, y: 150.0 + 50.0 * sin t, width: 10.0, height: 10.0 }
