module Gesso.Example.MouseAndScaling (main) where

import Prelude

import Data.Array (range)
import Data.Foldable (traverse_)
import Data.Int (toNumber, floor, round)
import Data.Maybe (Maybe(..))
import Data.Number (tau)
import Effect (Effect)
import Gesso (launch) as Gesso
import Gesso.Application (WindowMode(..), defaultBehavior) as GApp
import Gesso.Geometry (yTo, xTo, lengthTo, to)
import Gesso.Geometry (fromMouseEvent, Scalers, Point) as GGeo
import Gesso.Interactions (onMouseMove, onMouseDown, MouseInteraction) as GInt
import Gesso.Time (Delta) as GTime
import Gesso.State (States) as GSt
import Graphics.Canvas as Canvas

main :: Effect Unit
main = Gesso.launch
  { name: "test-app"
  , initialState
  , window: GApp.Fullscreen
  , viewBox: { x: -1.5, y: -1.5, width: 3.0, height: 3.0 }
  , behavior:
      GApp.defaultBehavior
        { render = render
        , interactions { mouse = [ mousePosition, mouseDown ] }
        }
  }

type State =
  { mousePos :: Maybe GGeo.Point
  , clicked :: Maybe GGeo.Point
  }

initialState :: State
initialState =
  { mousePos: Nothing
  , clicked: Nothing
  }

mousePosition :: GInt.MouseInteraction State
mousePosition = GInt.onMouseMove set
  where
  set event _ _ state =
    pure $ Just $ state { mousePos = Just $ GGeo.fromMouseEvent event }

mouseDown :: GInt.MouseInteraction State
mouseDown = GInt.onMouseDown set
  where
  set event _ _ state =
    pure $ Just $ state { clicked = Just $ GGeo.fromMouseEvent event }

render
  :: Canvas.Context2D
  -> GTime.Delta
  -> GGeo.Scalers
  -> GSt.States State
  -> Effect Unit
render context _ { canvas, drawing } { current: { clicked, mousePos } } = do
  clearBackground
  drawAxes
  drawGridLines
  drawMouseClicked clicked
  traverse_ drawMouseCursor mousePos
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context canvas.rect

  drawAxes :: Effect Unit
  drawAxes = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 0.015 `lengthTo` canvas
    drawCross ({ x: 0.0, y: 0.0 } `to` canvas) 1.0

  drawGridLines :: Effect Unit
  drawGridLines = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 0.005 `lengthTo` canvas
    traverse_ drawGridLine $ range 1 10

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (-n `xTo` canvas) (-1.0 `yTo` canvas)
      Canvas.lineTo context (-n `xTo` canvas) (1.0 `yTo` canvas)
      Canvas.moveTo context (n `xTo` canvas) (-1.0 `yTo` canvas)
      Canvas.lineTo context (n `xTo` canvas) (1.0 `yTo` canvas)
      Canvas.moveTo context (-1.0 `xTo` canvas) (-n `yTo` canvas)
      Canvas.lineTo context (1.0 `xTo` canvas) (-n `yTo` canvas)
      Canvas.moveTo context (-1.0 `xTo` canvas) (n `yTo` canvas)
      Canvas.lineTo context (1.0 `xTo` canvas) (n `yTo` canvas)
    where
    n = (_ / 10.0) <<< toNumber $ i

  drawMouseClicked :: Maybe GGeo.Point -> Effect Unit
  drawMouseClicked mxy = do
    Canvas.setFont context $ size <> "px 'Courier New'"
    Canvas.setFillStyle context "black"
    Canvas.setTextAlign context Canvas.AlignCenter
    Canvas.fillText context
      ("Clicked: (" <> text)
      (0.0 `xTo` canvas)
      (-1.1 `yTo` canvas)
    case mxy of
      Nothing -> pure unit
      Just p -> do
        Canvas.setStrokeStyle context "black"
        Canvas.setLineWidth context $ 0.01 `lengthTo` canvas
        Canvas.strokePath context do
          Canvas.arc context
            { x: p.x
            , y: p.y
            , radius: 0.05 `lengthTo` canvas
            , start: 0.0
            , end: tau
            , useCounterClockwise: false
            }
        drawCross p 0.05
    where
    size = show $ floor $ 0.2 `lengthTo` canvas

    x' = round3 <<< (_ `xTo` drawing)

    y' = round3 <<< (_ `yTo` drawing)

    round3 = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0)

    text = case mxy of
      Nothing -> "Nothing)"
      Just p -> show (x' p.x) <> ", " <> show (y' p.y) <> ")"

  drawMouseCursor :: GGeo.Point -> Effect Unit
  drawMouseCursor point = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 0.01 `lengthTo` canvas
    drawCross point 0.05

  drawCross :: { x :: Number, y :: Number } -> Number -> Effect Unit
  drawCross { x, y } length = do
    Canvas.strokePath context do
      let l = length `lengthTo` canvas
      Canvas.moveTo context (x - l) y
      Canvas.lineTo context (x + l) y
      Canvas.moveTo context x (y - l)
      Canvas.lineTo context x (y + l)
