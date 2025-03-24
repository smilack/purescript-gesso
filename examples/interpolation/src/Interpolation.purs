module Gesso.Example.Interpolation (main) where

import Prelude

import Data.Int (trunc, parity, Parity(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)
import Effect (Effect)
import Gesso (launch) as Gesso
import Gesso.Application (defaultBehavior, WindowMode(..)) as GApp
import Gesso.Geometry (Scalers, null) as GGeo
import Gesso.State (States, lerp) as GSt
import Gesso.Time (Delta, hz) as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main = Gesso.launch
  { name: "interpolation"
  , initialState
  , window: GApp.Fullscreen
  , viewBox: GGeo.null
  , behavior: GApp.defaultBehavior
      { render = render
      , fixed = { interval: GTime.hz 20.0, function: fixedUpdate }
      }
  }

type Ball =
  { x :: Number
  , vx :: Number
  , y :: Number
  , r :: Number
  }

type State = { ball :: Ball, seconds :: Number }

initialState :: State
initialState =
  { ball: { x: 75.0, y: 100.0, vx: 0.3, r: 50.0 }
  , seconds: 0.0
  }

fixedUpdate :: GTime.Delta -> GGeo.Scalers -> State -> Effect (Maybe State)
fixedUpdate { delta } { canvas } { ball, seconds } = do
  pure $ Just $
    { ball: move { min, max } delta ball
    , seconds: seconds + delta / 1000.0
    }
  where
  min = canvas.x
  max = min + canvas.width

move :: { min :: Number, max :: Number } -> Number -> Ball -> Ball
move { min, max } dt ball@{ x, vx, r } = ball { x = x', vx = vx' }
  where
  x' = x + vx * dt
  vx'
    | x' + r + vx > max = 0.0 - abs vx
    | x' - r + vx < min = abs vx
    | otherwise = vx

render
  :: Canvas.Context2D
  -> GTime.Delta
  -> GGeo.Scalers
  -> GSt.States State
  -> Effect Unit
render context _ _ { previous, current: { ball, seconds }, t } = do
  Canvas.setLineWidth context 10.0
  Canvas.setStrokeStyle context color
  Canvas.strokePath context do
    Canvas.arc context
      { x
      , y
      , radius: ball.r
      , start: 0.0
      , end: 2.0 * pi
      , useCounterClockwise: false
      }
  where
  { x, y, color } = case parity $ trunc $ seconds of
    Even ->
      { x: GSt.lerp { t, previous: previous.ball.x, current: ball.x }
      , y: GSt.lerp { t, previous: previous.ball.y, current: ball.y }
      , color: "black"
      }
    Odd ->
      { x: ball.x
      , y: ball.y
      , color: "white"
      }
