module Example.Interpolation.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application (StateDelta, defaultApp, WindowMode(..))
import Gesso.Canvas (Input)
import Gesso.Dimensions as GDims
import Gesso.Interactions as Interactions
import Gesso.Time (Delta, hz) as Time
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

type Object =
  { x :: Number
  , vx :: Number
  , y :: Number
  , vy :: Number
  }

type State =
  { smooth :: Object
  , jitter :: Object
  }

object :: { x :: Number, y :: Number } -> Object
object { x, y } = { x, y, vx: velocity, vy: velocity }

initialState :: State
initialState =
  { smooth: object { x: 25.0, y: 12.5 }
  , jitter: object { x: 75.0, y: 12.5 }
  }

canvasInput :: forall i o. Input State i o
canvasInput =
  { name: "interpolation"
  , localState: initialState
  , app: defaultApp
      { window = Fullscreen
      , render = render
      , fixed = { interval: Time.hz 30.0, function: fixedUpdate }
      }
  , viewBox: GDims.p1080
  , interactions: Interactions.default
  }

radius :: Number
radius = 25.0

velocity :: Number
velocity = 0.5

fixedUpdate :: Time.Delta -> GDims.Scaler -> State -> Effect (Maybe State)
fixedUpdate { delta } scale { smooth, jitter } = pure $ Just $
  { smooth: move borders delta smooth
  , jitter: move borders delta jitter
  }
  where
  borders = { min, max }
  min =
    { x: GDims.getX scale.screen
    , y: GDims.getY scale.screen
    }
  max =
    { x: min.x + GDims.getWidth scale.screen
    , y: min.y + GDims.getHeight scale.screen
    }

type Borders =
  { min ::
      { x :: Number
      , y :: Number
      }
  , max ::
      { x :: Number
      , y :: Number
      }
  }

move :: Borders -> Number -> Object -> Object
move { min, max } delta obj =
  { x: obj.x + delta * vx
  , y: obj.y + delta * vy
  , vx
  , vy
  }
  where
  vx = updateDirection { d: obj.x, min: min.x, max: max.x, v: obj.vx }
  vy = updateDirection { d: obj.y, min: min.y, max: max.y, v: obj.vy }

updateDirection
  :: { d :: Number, min :: Number, max :: Number, v :: Number } -> Number
updateDirection { d, min, max, v }
  | d + radius + v > max = (-velocity)
  | d - radius + v < min = velocity
  | otherwise = v

render
  :: Canvas.Context2D -> Time.Delta -> GDims.Scaler -> StateDelta State -> Effect Unit
render context { delta } scale { current, previous } = do
  Canvas.clearRect context (scale.toRectangle scale.screen)
  Canvas.setFillStyle context "black"
  Canvas.setFont context "32pt Arial"
  -- Canvas.
  Canvas.fillText context (show (1000.0 / delta)) 30.0 60.0
  Canvas.setFillStyle context "red"
  Canvas.fillPath context do
    Canvas.arc context
      { x: current.smooth.x
      , y: current.smooth.y
      , radius
      , start: 0.0
      , end: 2.0 * pi
      , useCounterClockwise: false
      }
  Canvas.setFillStyle context "blue"
  Canvas.fillPath context do
    Canvas.arc context
      { x: current.jitter.x
      , y: current.jitter.y
      , radius
      , start: 0.0
      , end: 2.0 * pi
      , useCounterClockwise: false
      }
-- where
-- pct = 
-- smooth =
--   { x: previous.smooth.x + delta * (current.smooth.x - )