module Example.ControllingBall.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as GApp
import Gesso.Canvas as GCan
import Gesso.Dimensions as GDims
import Gesso.Interactions as GInt
import Gesso.Time as GTime
import Graphics.Canvas as Canvas
import Math (pi)

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

type State
  = { x :: Number, vx :: Number, y :: Number, vy :: Number, radius :: Number }

canvasInput :: forall g i o. GCan.Input State g i o
canvasInput =
  { name: "bouncing-ball"
  , localState: { x: 0.0, vx: 1.0, y: 0.0, vy: 1.0, radius: 25.0 }
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fullscreen
            , render = Just $ GApp.continuous render
            , update = Just $ GApp.updateFn update
            }
  , viewBox: GDims.p1080
  , interactions: GInt.default
  }

update :: GTime.Delta -> GDims.Scaler -> State -> Maybe State
update _ scale { x, vx, y, vy, radius } = Just { x: x + vx', vx: vx', y: y + vy', vy: vy', radius }
  where
  xMin = GDims.getX scale.screen

  xMax = xMin + GDims.getWidth scale.screen

  vx' = updateV x radius xMin xMax vx

  yMin = GDims.getY scale.screen

  yMax = yMin + GDims.getHeight scale.screen

  vy' = updateV y radius yMin yMax vy

updateV :: Number -> Number -> Number -> Number -> Number -> Number
updateV t r min max vt
  | t + r + vt > max = -1.0
  | t - r + vt < min = 1.0
  | otherwise = vt

render :: State -> GTime.Delta -> GDims.Scaler -> Canvas.Context2D -> Effect Unit
render { x, y, radius } _ scale context = do
  Canvas.clearRect context (scale.toRectangle scale.screen)
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius, start: 0.0, end: 2.0 * pi }
