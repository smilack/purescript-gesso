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
  = { x :: Number, y :: Number, radius :: Number, keys :: Keys }

type Keys
  = { up :: Boolean, down :: Boolean, left :: Boolean, right :: Boolean }

canvasInput :: forall g i o. GCan.Input State g i o
canvasInput =
  { name: "controlling-ball"
  , localState: { x: 100.0, y: 100.0, radius: 25.0, keys: { up: false, down: false, left: false, right: false } }
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
    "ArrowUp" -> Just state { keys { up = true } }
    "ArrowDown" -> Just state { keys { down = true } }
    "ArrowLeft" -> Just state { keys { left = true } }
    "ArrowRight" -> Just state { keys { right = true } }
    _ -> Nothing

keyUp :: forall i. GInt.Interaction GEv.KeyboardEvent State i
keyUp = GInt.mkInteraction GEv.onKeyUp go
  where
  go :: GEv.KeyboardEvent -> GTime.Delta -> GDims.Scaler -> State -> Maybe State
  go event _ _ state = case KEv.key event of
    "ArrowUp" -> Just state { keys { up = false } }
    "ArrowDown" -> Just state { keys { down = false } }
    "ArrowLeft" -> Just state { keys { left = false } }
    "ArrowRight" -> Just state { keys { right = false } }
    _ -> Nothing

update :: GTime.Delta -> GDims.Scaler -> State -> Maybe State
update _ scale state@{ x, y, radius, keys: { up, down, left, right } } =
  Just
    state
      { x = updateP x radius xMin xMax vx
      , y = updateP y radius yMin yMax vy
      }
  where
  vx = getV left right

  xMin = GDims.getX scale.screen

  xMax = xMin + GDims.getWidth scale.screen

  vy = getV up down

  yMin = GDims.getY scale.screen

  yMax = yMin + GDims.getHeight scale.screen

getV :: Boolean -> Boolean -> Number
getV neg pos = case neg of
  false -> case pos of
    false -> 0.0
    true -> 1.0
  true -> case pos of
    false -> -1.0
    true -> 0.0

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
