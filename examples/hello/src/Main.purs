module Example.Hello.Main where

import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as Gesso.Application
import Gesso.Geometry as Gesso.Geometry
import Gesso.State as Gesso.State
import Gesso.Time as Gesso.Time
import Graphics.Canvas as Graphics.Canvas
import Prelude (Unit, unit)

main :: Effect Unit
main = Gesso.launch canvasInput

-- Gesso.Canvas.Input's type variables are:
--   - local state (state held in canvas and given to update/render)
--   - input (type used to send data into canvas)
--   - output (type canvas uses to send data out)
canvasInput :: forall i o. Gesso.Application.AppSpec Unit i o
canvasInput =
  { name: "hello"
  , initialState: unit
  , viewBox: { x: 0.0, y: 0.0, width: 1920.0, height: 1080.0 }
  , window: Gesso.Application.Fullscreen
  , behavior: Gesso.Application.defaultBehavior { render = render }
  }

render :: Graphics.Canvas.Context2D -> Gesso.Time.Delta -> Gesso.Geometry.Scalers -> Gesso.State.States Unit -> Effect Unit
render context _ _ _ = do
  Graphics.Canvas.fillText context "hello world" 500.0 500.0
