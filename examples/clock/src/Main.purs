module Example.Clock.Main where

import Prelude

import Data.Array (range)
import Data.Enum (fromEnum)
import Data.Foldable (sequence_)
import Data.Int (toNumber, floor)
import Data.Number (cos, sin, pi, tau)
import Data.Time as Time
import Effect (Effect)
import Effect.Now (nowTime) as Now
import Gesso (runGessoAff, awaitBody, run) as G
import Gesso.Application as GApp
import Gesso.Canvas (component, Input) as GC
import Gesso.Dimensions as GDim
import Gesso.Interactions as GInt
import Gesso.Time as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  G.runGessoAff do
    body <- G.awaitBody
    G.run GC.component input body

-- localState is unit because it's an input for run and needs to be
--   passed in. The rest can be open because we never need to use them
--   unless we call query or set the OutputMode to OutputFn
input :: forall i o. GC.Input Unit i o
input =
  { name: "test-app"
  , localState: unit
  , app:
      GApp.defaultApp
        { window = GApp.Fullscreen
        , render = render
        }
  , viewBox: GDim.p1080
  , interactions: GInt.default
  }

render :: Canvas.Context2D -> GTime.Delta -> GDim.Scaler -> GApp.StateDelta Unit -> Effect Unit
render context _ scale@{ toRectangle, screen, viewBox } _ = do
  -- Clear background
  Canvas.setFillStyle context "white"
  Canvas.fillRect context (toRectangle screen)
  drawFrame
  drawNumbers
  drawHashes
  { hour, minute, second } <- getTime
  drawHourHand hour minute
  drawMinuteHand minute second
  drawSecondHand second
  -- Center dot
  Canvas.setFillStyle context "#888888"
  Canvas.fillPath context do
    Canvas.arc context { x: clock.x, y: clock.y, start: 0.0, end: tau, radius: scale.width.toCr 15.0, useCounterClockwise: false }
  where
  clock =
    { x: scale.x.toCr $ (GDim.getWidth viewBox) / 2.0
    , y: scale.y.toCr $ (GDim.getHeight viewBox) / 2.0
    , r: scale.width.toCr $ 500.0
    }

  eta = pi / 2.0

  getTime :: Effect { hour :: Number, minute :: Number, second :: Number }
  getTime = do
    t <- Now.nowTime
    let
      hour = toNumber $ (_ `mod` 12) $ (_ + 7) $ fromEnum $ Time.hour t

      minute = toNumber $ fromEnum $ Time.minute t

      second = toNumber $ fromEnum $ Time.second t
    pure { hour, minute, second }

  drawFrame :: Effect Unit
  drawFrame = do
    Canvas.setFillStyle context "#eeeeee"
    Canvas.fillPath context do
      Canvas.arc context { x: clock.x, y: clock.y, start: 0.0, end: tau, radius: clock.r, useCounterClockwise: false }
    Canvas.setStrokeStyle context "#888888"
    Canvas.setLineWidth context $ scale.width.toCr 25.0
    Canvas.strokePath context do
      Canvas.arc context { x: clock.x, y: clock.y, start: 0.0, end: tau, radius: clock.r, useCounterClockwise: false }

  drawNumbers :: Effect Unit
  drawNumbers = do
    let
      size = floor $ scale.width.toCr 78.0
    Canvas.setFillStyle context "black"
    Canvas.setFont context $ show size <> "pt Georgia"
    Canvas.setTextAlign context Canvas.AlignCenter
    sequence_ $ map drawNumber $ range 1 12

  drawNumber :: Int -> Effect Unit
  drawNumber i = do
    let
      angle = (_ - eta) <<< (_ * (tau / 12.0)) <<< toNumber $ i `mod` 12

      x = clock.x + (0.775 * clock.r * cos angle)

      -- Graphics.Canvas doesn't have setTextBaseline, so push the numbers down a little
      y = clock.y + (0.775 * clock.r * sin angle + scale.height.toCr 30.0)
    Canvas.fillText context (show i) x y

  drawHashes :: Effect Unit
  drawHashes = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineCap context Canvas.Square
    sequence_ $ map drawHash $ range 0 59

  drawHash :: Int -> Effect Unit
  drawHash i = do
    if i `mod` 5 == 0 then
      Canvas.setLineWidth context $ scale.width.toCr 9.0
    else
      Canvas.setLineWidth context $ scale.width.toCr 3.0
    let
      angle = (_ * (tau / 60.0)) <<< toNumber $ i
    drawLineSegment angle 0.9 0.95

  drawHourHand :: Number -> Number -> Effect Unit
  drawHourHand hour minute = do
    let
      angle = (_ - eta) <<< (_ * (tau / 12.0)) $ (_ + (minute / 60.0)) $ hour
    Canvas.setLineCap context Canvas.Round
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ scale.width.toCr 16.0
    drawLineSegment angle (-0.1) 0.5

  drawMinuteHand :: Number -> Number -> Effect Unit
  drawMinuteHand minute second = do
    let
      angle = (_ - eta) <<< (_ * (tau / 60.0)) $ (_ + (second / 60.0)) $ minute
    Canvas.setLineCap context Canvas.Round
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ scale.width.toCr 16.0
    drawLineSegment angle (-0.1) 0.7

  drawSecondHand :: Number -> Effect Unit
  drawSecondHand second = do
    let
      angle = (_ - eta) <<< (_ * (tau / 60.0)) $ second
    Canvas.setLineCap context Canvas.Square
    Canvas.setStrokeStyle context "#DD0000"
    Canvas.setLineWidth context $ scale.width.toCr 7.0
    drawLineSegment angle (-0.2) 0.7
    Canvas.setLineWidth context $ scale.width.toCr 16.0
    Canvas.setLineCap context Canvas.Round
    drawLineSegment angle (-0.2) (-0.1)

  drawLineSegment :: Number -> Number -> Number -> Effect Unit
  drawLineSegment angle r1 r2 = do
    let
      x = clock.x + (r1 * clock.r * cos angle)

      x' = clock.x + (r2 * clock.r * cos angle)

      y = clock.y + (r1 * clock.r * sin angle)

      y' = clock.y + (r2 * clock.r * sin angle)
    Canvas.strokePath context do
      Canvas.moveTo context x y
      Canvas.lineTo context x' y'
