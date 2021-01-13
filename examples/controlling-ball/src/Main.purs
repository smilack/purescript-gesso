module Example.ControllingBall.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as GApp
import Gesso.Canvas as GCan
import Gesso.Dimensions as GDims
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
import Gesso.Time as GTime
import Graphics.Canvas as Canvas
import Math (pi)
import Web.UIEvent.KeyboardEvent as KEv

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

type State
  = { x :: Number, vx :: Number, y :: Number, vy :: Number, radius :: Number }

canvasInput :: forall g i o. GCan.Input State g i o
canvasInput =
  { name: "controlling-ball"
  , localState: { x: 100.0, vx: 0.0, y: 100.0, vy: 0.0, radius: 25.0 }
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fullscreen
            , render = Just $ GApp.continuous render
            , update = Just $ GApp.updateFn update
            }
  , viewBox: GDims.p1080
  , interactions: GInt.default { keyboard = [ keyDown, keyUp ], mouse = [ mouseDown ] }
  }

mouseDown :: forall i. GInt.Interaction GEv.MouseEvent State i
mouseDown = GInt.mkInteraction GEv.onMouseDown go
  where
  go :: GEv.MouseEvent -> GTime.Delta -> GDims.Scaler -> State -> Maybe State
  go event _ _ state =
    let
      point = GDims.fromMouseEvent event
    in
      Just state { x = GDims.getX point, y = GDims.getY point }

keyDown :: forall i. GInt.Interaction GEv.KeyboardEvent State i
keyDown = GInt.mkInteraction GEv.onKeyDown go
  where
  go :: GEv.KeyboardEvent -> GTime.Delta -> GDims.Scaler -> State -> Maybe State
  go event _ _ state = case KEv.key event of
    "ArrowUp" -> Just state { vy = -1.0 }
    "ArrowDown" -> Just state { vy = 1.0 }
    "ArrowLeft" -> Just state { vx = -1.0 }
    "ArrowRight" -> Just state { vx = 1.0 }
    _ -> Nothing

keyUp :: forall i. GInt.Interaction GEv.KeyboardEvent State i
keyUp = GInt.mkInteraction GEv.onKeyUp go
  where
  go :: GEv.KeyboardEvent -> GTime.Delta -> GDims.Scaler -> State -> Maybe State
  go event _ _ state = case KEv.key event of
    "ArrowUp" -> Just state { vy = 0.0 }
    "ArrowDown" -> Just state { vy = 0.0 }
    "ArrowLeft" -> Just state { vx = 0.0 }
    "ArrowRight" -> Just state { vx = 0.0 }
    _ -> Nothing

update :: GTime.Delta -> GDims.Scaler -> State -> Maybe State
update _ scale state@{ x, vx, y, vy, radius } =
  Just
    state
      { x = updateP x radius xMin xMax vx
      , y = updateP y radius yMin yMax vy
      }
  where
  xMin = GDims.getX scale.screen

  xMax = xMin + GDims.getWidth scale.screen

  yMin = GDims.getY scale.screen

  yMax = yMin + GDims.getHeight scale.screen

updateP :: Number -> Number -> Number -> Number -> Number -> Number
updateP p r min max v
  | p + r + v > max = max - r
  | p - r + v < min = min + r
  | otherwise = p + v

render :: State -> GTime.Delta -> GDims.Scaler -> Canvas.Context2D -> Effect Unit
render { x, y, radius } _ scale context = do
  Canvas.clearRect context (scale.toRectangle scale.screen)
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius, start: 0.0, end: 2.0 * pi }
