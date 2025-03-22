module Gesso.Example.Keyboard where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso (launch) as Gesso
import Gesso.Application (WindowMode(..), defaultBehavior) as GApp
import Gesso.Geometry (null, Scalers) as GGeo
import Gesso.Interactions (KeyboardInteraction, KeyboardEvent, onKeyUp, onKeyDown) as GInt
import Gesso.State (States) as GSt
import Gesso.Time (Delta) as GTime
import Graphics.Canvas as Canvas
import Web.UIEvent.KeyboardEvent (key) as KEv

main :: Effect Unit
main = Gesso.launch
  { name: "keyboard"
  , initialState
  , viewBox: GGeo.null
  , window: GApp.Fullscreen
  , behavior: GApp.defaultBehavior
      { render = render
      , update = update
      , interactions { keyboard = [ keyDown, keyUp ] }
      }
  }

type Keys =
  { up :: Boolean
  , down :: Boolean
  , left :: Boolean
  , right :: Boolean
  }

type State = { x :: Number, y :: Number, keys :: Keys }

v :: Number
v = 3.0

halfSide :: Number
halfSide = 50.0

initialState :: State
initialState =
  { x: 1.5 * halfSide
  , y: 1.5 * halfSide
  , keys: { up: false, down: false, left: false, right: false }
  }

keyDown :: GInt.KeyboardInteraction State
keyDown = GInt.onKeyDown $ setKey true

keyUp :: GInt.KeyboardInteraction State
keyUp = GInt.onKeyUp $ setKey false

setKey
  :: Boolean
  -> GInt.KeyboardEvent
  -> GTime.Delta
  -> GGeo.Scalers
  -> State
  -> Effect (Maybe State)
setKey val event _ _ state = pure $ case KEv.key event of
  "ArrowUp" -> Just state { keys { up = val } }
  "ArrowDown" -> Just state { keys { down = val } }
  "ArrowLeft" -> Just state { keys { left = val } }
  "ArrowRight" -> Just state { keys { right = val } }
  _ -> Nothing

update :: GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
update _ { canvas } state@{ x, y, keys: { up, down, left, right } } =
  pure $ Just state
    { x = updatePosition x canvas.x (canvas.x + canvas.width) vx
    , y = updatePosition y canvas.y (canvas.y + canvas.height) vy
    }
  where
  vx = getV left right
  vy = getV up down

getV :: Boolean -> Boolean -> Number
getV neg pos
  | neg && pos = 0.0
  | neg = -v
  | pos = v
  | otherwise = 0.0

updatePosition :: Number -> Number -> Number -> Number -> Number
updatePosition pos min max velocity
  | pos + halfSide + velocity > max = pos
  | pos - halfSide + velocity < min = pos
  | otherwise = pos + velocity

render
  :: Canvas.Context2D
  -> GTime.Delta
  -> GGeo.Scalers
  -> GSt.States State
  -> Effect Unit
render context _ { canvas } { current: { x, y } } = do
  Canvas.clearRect context canvas.rect
  Canvas.setFillStyle context "blue"
  Canvas.fillRect context
    { x: x - halfSide
    , y: y - halfSide
    , width: halfSide * 2.0
    , height: halfSide * 2.0
    }
