module Example.Hello.Main where

import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as Gesso.Application
import Gesso.Canvas as Gesso.Canvas
import Gesso.Dimensions as Gesso.Dimensions
import Gesso.Interactions as Gesso.Interactions
import Gesso.Time as Gesso.Time
import Gesso.Util.Lerp as Gesso.Util.Lerp
import Graphics.Canvas as Graphics.Canvas
import Prelude (Unit, unit, bind)

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

-- Gesso.Canvas.Input's type variables are:
--   - local state (state held in canvas and given to update/render)
--   - input (type used to send data into canvas)
--   - output (type canvas uses to send data out)
canvasInput :: forall i o. Gesso.Canvas.Input Unit i o
canvasInput =
  { name: "hello"
  , localState: unit
  , app:
      Gesso.Application.defaultApp
        { window = Gesso.Application.Fullscreen
        , render = render
        }
  , viewBox: Gesso.Dimensions.p1080
  , interactions: Gesso.Interactions.default
  }

render :: Graphics.Canvas.Context2D -> Gesso.Time.Delta -> Gesso.Dimensions.Scaler -> Gesso.Util.Lerp.Lerp Unit -> Effect Unit
render context _ _ _ = do
  Graphics.Canvas.fillText context "hello world" 500.0 500.0
