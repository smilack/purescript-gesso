module Example.UnitGrid.Main where

import Prelude

import Data.Array (range)
import Data.Foldable (sequence_, traverse_)
import Data.Int (toNumber, floor, round)
import Data.Maybe (Maybe(..))
import Data.Number (tau)
import Effect (Effect)
import Gesso (runGessoAff, awaitBody, run) as G
import Gesso.Application as GApp
import Gesso.Canvas (component, Input) as GC
import Gesso.Geometry as GGeo
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
import Gesso.Geometry ((^@), (>@), (-@), to)
import Gesso.Time as GTime
import Gesso.Util.Lerp as GLerp
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  G.runGessoAff do
    body <- G.awaitBody
    G.run GC.component input body

type LocalState =
  { mousePos :: Maybe GGeo.Point
  , clicked :: Maybe GGeo.Point
  }

input :: forall i o. GC.Input LocalState i o
input =
  { name: "test-app"
  , localState: { mousePos: Nothing, clicked: Nothing }
  , app:
      GApp.defaultApp
        { window = GApp.Fullscreen
        , render = render
        }
  , viewBox: { x: -1.5, y: 1.5, width: 3.0, height: 3.0 }
  , interactions: GInt.default { mouse = [ GInt.mousePosition, mouseDown ] }
  }

mouseDown
  :: forall r
   . GInt.Interaction GEv.MouseEvent { clicked :: Maybe GGeo.Point | r }
mouseDown = GInt.Interaction GEv.onMouseDown getMousePos
  where
  getMousePos event _ _ state = pure $ Just state { clicked = Just $ GInt.fromMouseEvent event }

render :: Canvas.Context2D -> GTime.Delta -> GGeo.Scalers -> GLerp.Lerp LocalState -> Effect Unit
render context _ { canvas, drawing } { new: { clicked, mousePos } } = do
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
    Canvas.setLineWidth context $ 0.015 -@ canvas
    drawCross ({ x: 0.0, y: 0.0 } `to` canvas) 1.0

  drawGridLines :: Effect Unit
  drawGridLines = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 0.005 -@ canvas
    sequence_ $ map drawGridLine $ range 1 10

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (-n >@ canvas) (-1.0 ^@ canvas)
      Canvas.lineTo context (-n >@ canvas) (1.0 ^@ canvas)
      Canvas.moveTo context (n >@ canvas) (-1.0 ^@ canvas)
      Canvas.lineTo context (n >@ canvas) (1.0 ^@ canvas)
      Canvas.moveTo context (-1.0 >@ canvas) (-n ^@ canvas)
      Canvas.lineTo context (1.0 >@ canvas) (-n ^@ canvas)
      Canvas.moveTo context (-1.0 >@ canvas) (n ^@ canvas)
      Canvas.lineTo context (1.0 >@ canvas) (n ^@ canvas)
    where
    n = (_ / 10.0) <<< toNumber $ i

  drawMouseClicked :: Maybe GGeo.Point -> Effect Unit
  drawMouseClicked mxy = do
    Canvas.setFont context $ size <> "px 'Courier New'"
    Canvas.setFillStyle context "black"
    Canvas.setTextAlign context Canvas.AlignCenter
    Canvas.fillText context ("Clicked: (" <> text) (0.0 >@ canvas) (-1.1 ^@ canvas)
    case mxy of
      Nothing -> pure unit
      Just p -> do
        Canvas.setStrokeStyle context "black"
        Canvas.setLineWidth context $ 0.01 -@ canvas
        Canvas.strokePath context do
          Canvas.arc context { x: p.x, y: p.y, radius: 0.05 -@ canvas, start: 0.0, end: tau, useCounterClockwise: false }
        drawCross p 0.05
    where
    size = show $ floor $ 0.2 -@ canvas

    x' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< (_ >@ drawing)

    y' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< (_ ^@ drawing)

    text = case mxy of
      Nothing -> "Nothing)"
      Just p -> show (x' p.x) <> ", " <> show (y' p.y) <> ")"

  drawMouseCursor :: GGeo.Point -> Effect Unit
  drawMouseCursor point = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ 0.01 -@ canvas
    drawCross point 0.05

  drawCross :: { x :: Number, y :: Number } -> Number -> Effect Unit
  drawCross { x, y } length = do
    Canvas.strokePath context do
      let
        l = length -@ canvas
      Canvas.moveTo context (x - l) y
      Canvas.lineTo context (x + l) y
      Canvas.moveTo context x (y - l)
      Canvas.lineTo context x (y + l)
