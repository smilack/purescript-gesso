module Gesso.Example.AnalogClock (main) where

import Prelude

import Data.Array (range)
import Data.Enum (fromEnum)
import Data.Foldable (sequence_)
import Data.Int (toNumber, floor)
import Data.Number (cos, sin, pi, tau)
import Data.Time (hour, minute, second) as Time
import Effect (Effect)
import Effect.Now (nowTime) as Now
import Gesso (launch) as Gesso
import Gesso.Application (WindowMode(..), defaultBehavior) as GApp
import Gesso.Geometry (lengthTo, to)
import Gesso.Geometry (Scalers) as GGeo
import Gesso.State (States) as GSt
import Gesso.Time (Delta) as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main = Gesso.launch
  { name: "analog-clock"
  , initialState: unit
  , window: GApp.Fullscreen
  , viewBox: { x: 0.0, y: 0.0, width: 1100.0, height: 1100.0 }
  , behavior: GApp.defaultBehavior { render = render }
  }

render
  :: Canvas.Context2D
  -> GTime.Delta
  -> GGeo.Scalers
  -> GSt.States Unit
  -> Effect Unit
render context _ { canvas, drawing } _ = do
  -- Clear background
  Canvas.setFillStyle context "white"
  Canvas.fillRect context canvas.rect
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
    Canvas.arc context
      { x: clock.x
      , y: clock.y
      , start: 0.0
      , end: tau
      , radius: 15.0 `lengthTo` canvas
      , useCounterClockwise: false
      }
  where
  clock =
    { x: drawing.width / 2.0
    , y: drawing.height / 2.0
    , r: 500.0
    } `to` canvas

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
      Canvas.arc context
        { x: clock.x
        , y: clock.y
        , start: 0.0
        , end: tau
        , radius: clock.r
        , useCounterClockwise: false
        }
    Canvas.setStrokeStyle context "#888888"
    Canvas.setLineWidth context $ 25.0 `lengthTo` canvas
    Canvas.strokePath context do
      Canvas.arc context
        { x: clock.x
        , y: clock.y
        , start: 0.0
        , end: tau
        , radius: clock.r
        , useCounterClockwise: false
        }

  drawNumbers :: Effect Unit
  drawNumbers = do
    let
      size = floor $ 78.0 `lengthTo` canvas
    Canvas.setFillStyle context "black"
    Canvas.setFont context $ show size <> "pt Georgia"
    Canvas.setTextAlign context Canvas.AlignCenter
    sequence_ $ map drawNumber $ range 1 12

  drawNumber :: Int -> Effect Unit
  drawNumber i = do
    let
      angle = (_ - eta) <<< (_ * (tau / 12.0)) <<< toNumber $ i `mod` 12

      x = clock.x + (0.775 * clock.r * cos angle)

      -- Graphics.Canvas doesn't have setTextBaseline, so push the numbers down
      -- a little bit
      y = clock.y + (0.775 * clock.r * sin angle + (30.0 `lengthTo` canvas))
    Canvas.fillText context (show i) x y

  drawHashes :: Effect Unit
  drawHashes = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineCap context Canvas.Square
    sequence_ $ map drawHash $ range 0 59

  drawHash :: Int -> Effect Unit
  drawHash i = do
    if i `mod` 5 == 0 then
      Canvas.setLineWidth context $ 9.0 `lengthTo` canvas
    else
      Canvas.setLineWidth context $ 3.0 `lengthTo` canvas
    let
      angle = (_ * (tau / 60.0)) <<< toNumber $ i
    drawLineSegment angle 0.9 0.95

  drawHourHand :: Number -> Number -> Effect Unit
  drawHourHand hour minute = do
    let
      angle = (_ - eta) <<< (_ * (tau / 12.0)) $ (_ + (minute / 60.0)) $ hour
    Canvas.setLineCap context Canvas.Round
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 16.0 `lengthTo` canvas
    drawLineSegment angle (-0.1) 0.5

  drawMinuteHand :: Number -> Number -> Effect Unit
  drawMinuteHand minute second = do
    let
      angle = (_ - eta) <<< (_ * (tau / 60.0)) $ (_ + (second / 60.0)) $ minute
    Canvas.setLineCap context Canvas.Round
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 16.0 `lengthTo` canvas
    drawLineSegment angle (-0.1) 0.7

  drawSecondHand :: Number -> Effect Unit
  drawSecondHand second = do
    let
      angle = (_ - eta) <<< (_ * (tau / 60.0)) $ second
    Canvas.setLineCap context Canvas.Square
    Canvas.setStrokeStyle context "#DD0000"
    Canvas.setLineWidth context $ 7.0 `lengthTo` canvas
    drawLineSegment angle (-0.2) 0.7
    Canvas.setLineWidth context $ 16.0 `lengthTo` canvas
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
