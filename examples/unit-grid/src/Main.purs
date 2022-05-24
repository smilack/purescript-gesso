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
import Gesso.Dimensions as GDim
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
import Gesso.Time as GTime
import Graphics.Canvas as Canvas

main :: Effect Unit
main =
  G.runGessoAff do
    body <- G.awaitBody
    G.run GC.component input body

type LocalState =
  { mousePos :: Maybe GDim.Point
  , clicked :: Maybe GDim.Point
  }

input :: forall i o. GC.Input LocalState i o
input =
  { name: "test-app"
  , localState: { mousePos: Nothing, clicked: Nothing }
  , app:
      GApp.defaultApp
        { window = GApp.fullscreen
        , render = render
        }
  , viewBox:
      GDim.fromPointAndSize
        (GDim.fromXAndY { x: -1.5, y: -1.5 })
        (GDim.fromWidthAndHeight { width: 3.0, height: 3.0 })
  , interactions: GInt.default { mouse = [ GInt.mousePosition, mouseDown ] }
  }

mouseDown
  :: forall r i
   . GInt.Interaction GEv.MouseEvent { clicked :: Maybe GDim.Point | r } i
mouseDown = GInt.mkInteraction GEv.onMouseDown getMousePos
  where
  getMousePos event _ _ state = Just state { clicked = Just $ GDim.fromMouseEvent event }

render :: LocalState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
render { clicked, mousePos } _ scale context = do
  clearBackground
  drawAxes
  drawGridLines
  drawMouseClicked clicked
  traverse_ drawMouseCursor mousePos
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context (scale.toRectangle scale.screen)

  drawAxes :: Effect Unit
  drawAxes = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ scale.width.toCr 0.015
    drawCross (scale.x.toCr 0.0) (scale.y.toCr 0.0) 1.0

  drawGridLines :: Effect Unit
  drawGridLines = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ scale.width.toCr 0.005
    sequence_ $ map drawGridLine $ range 1 10

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (scale.x.toCr $ -n) (scale.y.toCr $ -1.0)
      Canvas.lineTo context (scale.x.toCr $ -n) (scale.y.toCr $ 1.0)
      Canvas.moveTo context (scale.x.toCr n) (scale.y.toCr $ -1.0)
      Canvas.lineTo context (scale.x.toCr n) (scale.y.toCr $ 1.0)
      Canvas.moveTo context (scale.x.toCr $ -1.0) (scale.y.toCr $ -n)
      Canvas.lineTo context (scale.x.toCr $ 1.0) (scale.y.toCr $ -n)
      Canvas.moveTo context (scale.x.toCr $ -1.0) (scale.y.toCr n)
      Canvas.lineTo context (scale.x.toCr $ 1.0) (scale.y.toCr n)
    where
    n = (_ / 10.0) <<< toNumber $ i

  drawMouseClicked :: Maybe GDim.Point -> Effect Unit
  drawMouseClicked mxy = do
    Canvas.setFont context $ size <> "px 'Courier New'"
    Canvas.setFillStyle context "black"
    Canvas.setTextAlign context Canvas.AlignCenter
    Canvas.fillText context ("Clicked: (" <> text) (scale.x.toCr $ 0.0) (scale.y.toCr $ -1.1)
    case mxy of
      Nothing -> pure unit
      Just p -> do
        Canvas.setStrokeStyle context "black"
        Canvas.setLineWidth context $ scale.width.toCr 0.01
        Canvas.strokePath context do
          Canvas.arc context { x: GDim.getX p, y: GDim.getY p, radius: scale.width.toCr 0.05, start: 0.0, end: tau }
        drawCross (GDim.getX p) (GDim.getY p) 0.05
    where
    size = show $ floor $ scale.width.toCr 0.2

    x' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< scale.x.toVb

    y' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< scale.y.toVb

    text = case mxy of
      Nothing -> "Nothing)"
      Just p -> show (x' $ GDim.getX p) <> ", " <> show (y' $ GDim.getY p) <> ")"

  drawMouseCursor :: GDim.Point -> Effect Unit
  drawMouseCursor point = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ scale.width.toCr 0.01
    drawCross (GDim.getX point) (GDim.getY point) 0.05

  drawCross :: Number -> Number -> Number -> Effect Unit
  drawCross x y length = do
    Canvas.strokePath context do
      Canvas.moveTo context (x - scale.width.toCr length) y
      Canvas.lineTo context (x + scale.width.toCr length) y
      Canvas.moveTo context x (y - scale.height.toCr length)
      Canvas.lineTo context x (y + scale.height.toCr length)
