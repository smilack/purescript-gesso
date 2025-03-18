module Example.ControllingBall.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as GApp
import Gesso.Canvas as GCan
import Gesso.Geometry as GGeo
import Gesso.Interactions as GInt
import Gesso.Time as GTime
import Gesso.Util.Lerp as GLerp
import Graphics.Canvas as Canvas
import Web.UIEvent.KeyboardEvent as KEv

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

type State = { x :: Number, y :: Number, radius :: Number, keys :: Keys }

type Keys = { up :: Boolean, down :: Boolean, left :: Boolean, right :: Boolean }

canvasInput :: forall i o. GCan.Input State i o
canvasInput =
  { name: "controlling-ball"
  , localState: { x: 100.0, y: 100.0, radius: 25.0, keys: { up: false, down: false, left: false, right: false } }
  , app:
      GApp.defaultApp
        { window = GApp.Fullscreen
        , render = render
        , update = update
        }
  , viewBox: { x: 0.0, y: 0.0, width: 1920.0, height: 1080.0 }
  , interactions: GInt.default { keyboard = [ keyDown, keyUp ], mouse = [ mouseDown ] }
  }

mouseDown :: GInt.Interaction GInt.MouseEvent State
mouseDown = GInt.onMouseDown go
  where
  go :: GInt.MouseEvent -> GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
  go event _ _ state = pure $
    let
      point = GGeo.fromMouseEvent event
    in
      Just state { x = point.x, y = point.y }

keyDown :: GInt.Interaction GInt.KeyboardEvent State
keyDown = GInt.onKeyDown go
  where
  go :: GInt.KeyboardEvent -> GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
  go event _ _ state = pure $ case KEv.key event of
    "ArrowUp" -> Just state { keys { up = true } }
    "ArrowDown" -> Just state { keys { down = true } }
    "ArrowLeft" -> Just state { keys { left = true } }
    "ArrowRight" -> Just state { keys { right = true } }
    _ -> Nothing

keyUp :: GInt.Interaction GInt.KeyboardEvent State
keyUp = GInt.onKeyUp go
  where
  go :: GInt.KeyboardEvent -> GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
  go event _ _ state = pure $ case KEv.key event of
    "ArrowUp" -> Just state { keys { up = false } }
    "ArrowDown" -> Just state { keys { down = false } }
    "ArrowLeft" -> Just state { keys { left = false } }
    "ArrowRight" -> Just state { keys { right = false } }
    _ -> Nothing

update :: GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
update _ { canvas } state@{ x, y, radius, keys: { up, down, left, right } } = pure $
  Just
    state
      { x = updateP x radius xMin xMax vx
      , y = updateP y radius yMin yMax vy
      }
  where
  vx = getV left right

  xMin = canvas.x

  xMax = xMin + canvas.width

  vy = getV up down

  yMin = canvas.y

  yMax = yMin + canvas.height

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

render :: Canvas.Context2D -> GTime.Delta -> GGeo.Scalers -> GLerp.Lerp State -> Effect Unit
render context _ { canvas } { new: { x, y, radius } } = do
  Canvas.clearRect context canvas.rect
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context { x, y, radius, start: 0.0, end: 2.0 * pi, useCounterClockwise: false }
