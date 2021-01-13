module Example.BouncingBall.Main where

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
  = { x :: Number, vx :: Number, y :: Number, vy :: Number }

canvasInput :: forall g i o. GCan.Input State g i o
canvasInput =
  { name: "bouncing-ball"
  , localState: { x: 0.0, vx: 1.0, y: 0.0, vy: 1.0 }
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
update _ scale { x, vx, y, vy } = Just { x: x + vx', vx: vx', y: y + vy', vy: vy' }
  where
  xMin = GDims.getX scale.screen

  xMax = xMin + GDims.getWidth scale.screen

  vx' = updateV x xMin xMax vx

  yMin = GDims.getY scale.screen

  yMax = yMin + GDims.getHeight scale.screen

  vy' = updateV y yMin yMax vy

updateV :: Number -> Number -> Number -> Number -> Number
updateV t min max vt
  | t + vt > max = -1.0
  | t + vt < min = 1.0
  | otherwise = vt

render :: State -> GTime.Delta -> GDims.Scaler -> Canvas.Context2D -> Effect Unit
render { x, y } _ scale context = do
  Canvas.clearRect context (scale.toRectangle scale.screen)
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius: 25.0, start: 0.0, end: 2.0 * pi }
