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
  = { x :: Number, y :: Number }

canvasInput :: forall g i o. GCan.Input State g i o
canvasInput =
  { name: "bouncing-ball"
  , localState: { x: 0.0, y: 0.0 }
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
update _ _ state = Just state { x = state.x + 1.0, y = state.y + 1.0 }

render :: State -> GTime.Delta -> GDims.Scaler -> Canvas.Context2D -> Effect Unit
render { x, y } _ _ context = do
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius: 25.0, start: 0.0, end: 2.0 * pi }
