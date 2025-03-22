module Gesso.Example.Bounce (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Effect (Effect)
import Gesso (launch) as Gesso
import Gesso.Application (WindowMode(..), defaultBehavior) as GApp
import Gesso.Geometry (null, Scalers) as GGeo
import Gesso.State (States) as GSt
import Gesso.Time (Delta) as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main = Gesso.launch
  { name: "bounce"
  , initialState
  , viewBox: GGeo.null
  , window: GApp.Fullscreen
  , behavior: GApp.defaultBehavior
      { render = render
      , update = update
      }
  }

type State =
  { x :: Number
  , vx :: Number
  , y :: Number
  , vy :: Number
  }

v :: Number
v = 2.0

radius :: Number
radius = 50.0

initialState :: State
initialState =
  { x: radius
  , vx: v
  , y: radius
  , vy: v
  }

update :: GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
update _ { canvas } { x, vx, y, vy } = pure $ Just $
  { x: x + vx'
  , vx: vx'
  , y: y + vy'
  , vy: vy'
  }
  where
  vx' = updateV x canvas.x (canvas.x + canvas.width) vx
  vy' = updateV y canvas.y (canvas.y + canvas.height) vy

updateV :: Number -> Number -> Number -> Number -> Number
updateV position min max velocity
  | position + radius + velocity > max = -v
  | position - radius + velocity < min = v
  | otherwise = velocity

render
  :: Canvas.Context2D
  -> GTime.Delta
  -> GGeo.Scalers
  -> GSt.States State
  -> Effect Unit
render context _ { canvas } { current: { x, y } } = do
  Canvas.clearRect context canvas.rect
  Canvas.setFillStyle context "red"
  Canvas.fillPath context $
    Canvas.arc context
      { x, y, radius, start: 0.0, end: 2.0 * pi, useCounterClockwise: false }
