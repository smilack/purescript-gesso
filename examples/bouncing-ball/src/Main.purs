module Example.BouncingBall.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as GApp
import Gesso.Geometry as GGeo
import Gesso.State as GSt
import Gesso.Time as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main = Gesso.launch canvasInput

type State =
  { x :: Number, vx :: Number, y :: Number, vy :: Number, radius :: Number }

canvasInput :: forall i o. GApp.AppSpec State i o
canvasInput =
  { name: "bouncing-ball"
  , initialState: { x: 0.0, vx: 1.0, y: 0.0, vy: 1.0, radius: 25.0 }
  , viewBox: { x: 0.0, y: 0.0, width: 1920.0, height: 1080.0 }
  , window: GApp.Fullscreen
  , behavior: GApp.defaultBehavior
      { render = render
      , update = update
      }
  }

update :: GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
update _ { canvas } { x, vx, y, vy, radius } = pure $
  Just { x: x + vx', vx: vx', y: y + vy', vy: vy', radius }
  where
  xMin = canvas.x

  xMax = xMin + canvas.width

  vx' = updateV x radius xMin xMax vx

  yMin = canvas.y

  yMax = yMin + canvas.height

  vy' = updateV y radius yMin yMax vy

updateV :: Number -> Number -> Number -> Number -> Number -> Number
updateV position radius min max velocity
  | position + radius + velocity > max = -1.0
  | position - radius + velocity < min = 1.0
  | otherwise = velocity

render
  :: Canvas.Context2D -> GTime.Delta -> GGeo.Scalers -> GSt.States State -> Effect Unit
render context _ { canvas } { current: { x, y, radius } } = do
  Canvas.clearRect context canvas.rect
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius, start: 0.0, end: 2.0 * pi, useCounterClockwise: false }
