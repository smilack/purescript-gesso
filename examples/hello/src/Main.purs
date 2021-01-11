module Example.Hello.Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as Gesso.Application
import Gesso.Canvas as Gesso.Canvas
import Gesso.Dimensions as Gesso.Dimensions
import Gesso.Interactions as Gesso.Interactions
import Gesso.Time as Gesso.Time
import Graphics.Canvas as Canvas
import Prelude (Unit, unit, bind, discard, ($))

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

-- Gesso.Canvas.Input's type variables are:
--   - local state (state held in canvas and given to update/render)
--   - global state (globally accessible state or state of parent component)
--   - input (type used to send data into canvas)
--   - output (type canvas uses to send data out)
canvasInput :: forall g i o. Gesso.Canvas.Input Unit g i o
canvasInput =
  { name: "hello"
  , localState: unit
  , app:
      Gesso.Application.mkApplication
        $ Gesso.Application.defaultApp
            { window = Gesso.Application.fullscreen
            , render = Just $ Gesso.Application.continuous render
            }
  , viewBox: Gesso.Dimensions.p1080
  , interactions: Gesso.Interactions.default
  }

render :: Unit -> Gesso.Time.Delta -> Gesso.Dimensions.Scaler -> Canvas.Context2D -> Effect Unit
render _ _ _ context = do
  Canvas.setFillStyle context "black"
  Canvas.fillText context "hello world" 500.0 500.0
