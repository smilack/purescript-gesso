module Example.Interpolation.Main where

import Prelude

import Data.Int (trunc, parity, Parity(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application (defaultApp, WindowMode(..))
import Gesso.Canvas (Input)
import Gesso.Dimensions as GDims
import Gesso.Interactions as Interactions
import Gesso.Time (Delta, hz) as Time
import Gesso.Util.Lerp (Lerp, lerp)
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

type Ball =
  { x :: Number
  , vx :: Number
  , y :: Number
  , r :: Number
  }

type State = { ball :: Ball, seconds :: Number }

initialState :: State
initialState = { ball: { x: 75.0, y: 100.0, vx: 0.3, r: 50.0 }, seconds: 0.0 }

canvasInput :: forall i o. Input State i o
canvasInput =
  { name: "interpolation"
  , localState: initialState
  , app: defaultApp
      { window = Fullscreen
      , render = render
      , fixed = { interval: Time.hz 20.0, function: fixedUpdate }
      }
  , viewBox: GDims.p1080
  , interactions: Interactions.default
  }

fixedUpdate :: Time.Delta -> GDims.Scaler -> State -> Effect (Maybe State)
fixedUpdate { delta } scale { ball, seconds } = do
  pure $ Just $
    { ball: move { min, max } delta ball
    , seconds: seconds + delta / 1000.0
    }
  where
  min = GDims.getX scale.screen
  max = min + GDims.getWidth scale.screen

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
  -> Time.Delta
  -> GDims.Scaler
  -> Lerp State
  -> Effect Unit
render context _ _ { old, new: { ball, seconds }, t } = do
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
      { x: lerp { t, old: old.ball.x, new: ball.x }
      , y: lerp { t, old: old.ball.y, new: ball.y }
      , color: "black"
      }
    Odd ->
      { x: ball.x
      , y: ball.y
      , color: "white"
      }
