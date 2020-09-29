module Main where

import Prelude
import Data.Array (range)
import Data.Foldable (sequence_, traverse_)
import Data.Int (toNumber, floor, round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso (runGessoAff, awaitBody, run) as G
import Gesso.Application as GApp
import Gesso.Canvas (component, Input) as GC
import Gesso.Dimensions as GDim
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
import Gesso.Time as GTime
import Graphics.Canvas as Canvas
import Math (tau)

main :: Effect Unit
main =
  G.runGessoAff do
    body <- G.awaitBody
    G.run GC.component input body

type AppState
  = { mousePos :: Maybe { x :: Number, y :: Number }
    , clicked :: Maybe { x :: Number, y :: Number }
    }

input :: GC.Input AppState
input =
  { name: "test-app"
  , appState: { mousePos: Nothing, clicked: Nothing }
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fullscreen
            , render = Just $ GApp.continuous render
            }
  , viewBox:
      GDim.fromPointAndSize
        (GDim.fromXAndY { x: -1.5, y: -1.5 })
        (GDim.fromWidthAndHeight { width: 3.0, height: 3.0 })
  , interactions: GInt.default { mouse = [ GInt.mousePosition, mouseDown ] }
  }

mouseDown ::
  forall r i.
  GInt.Interaction GEv.MouseEvent { clicked :: Maybe { x :: Number, y :: Number } | r } i
mouseDown = GInt.mkInteraction GEv.onMouseDown getMousePos
  where
  getMousePos event state =
    let
      point = GDim.fromMouseEvent event
    in
      state { clicked = Just { x: GDim.getX point, y: GDim.getY point } }

render :: AppState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
render { clicked, mousePos } _ { x_, y_, w_, h_, screen, toVb } context = do
  clearBackground
  drawAxes
  drawGridLines
  drawMouseClicked clicked
  traverse_ drawMouseCursor mousePos
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context screen

  drawAxes :: Effect Unit
  drawAxes = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ w_ 0.015
    drawCross (x_ 0.0) (y_ 0.0) 1.0

  drawGridLines :: Effect Unit
  drawGridLines = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ w_ 0.005
    sequence_ $ map drawGridLine $ range 1 10

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (x_ $ -n) (y_ $ -1.0)
      Canvas.lineTo context (x_ $ -n) (y_ $ 1.0)
      Canvas.moveTo context (x_ n) (y_ $ -1.0)
      Canvas.lineTo context (x_ n) (y_ $ 1.0)
      Canvas.moveTo context (x_ $ -1.0) (y_ $ -n)
      Canvas.lineTo context (x_ $ 1.0) (y_ $ -n)
      Canvas.moveTo context (x_ $ -1.0) (y_ n)
      Canvas.lineTo context (x_ $ 1.0) (y_ n)
    where
    n = (_ / 10.0) <<< toNumber $ i

  drawMouseClicked :: Maybe { x :: Number, y :: Number } -> Effect Unit
  drawMouseClicked mxy = do
    Canvas.setFont context $ size <> "px 'Courier New'"
    Canvas.setFillStyle context "black"
    Canvas.setTextAlign context Canvas.AlignCenter
    Canvas.fillText context ("Clicked: (" <> text) (x_ $ 0.0) (y_ $ -1.1)
    case mxy of
      Nothing -> pure unit
      Just { x, y } -> do
        Canvas.setStrokeStyle context "black"
        Canvas.setLineWidth context $ w_ 0.01
        Canvas.strokePath context do
          Canvas.arc context { x, y, radius: w_ 0.05, start: 0.0, end: tau }
        drawCross x y 0.05
    where
    size = show $ floor $ w_ 0.2

    x' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< toVb.x'

    y' = (_ / 1000.0) <<< toNumber <<< round <<< (_ * 1000.0) <<< toVb.y'

    text = case mxy of
      Nothing -> "Nothing)"
      Just { x, y } -> show (x' x) <> ", " <> show (y' y) <> ")"

  drawMouseCursor :: { x :: Number, y :: Number } -> Effect Unit
  drawMouseCursor { x, y } = do
    Canvas.setStrokeStyle context "black"
    Canvas.setLineWidth context $ w_ 0.01
    drawCross x y 0.05

  drawCross :: Number -> Number -> Number -> Effect Unit
  drawCross x y length = do
    Canvas.strokePath context do
      Canvas.moveTo context (x - w_ length) y
      Canvas.lineTo context (x + w_ length) y
      Canvas.moveTo context x (y - h_ length)
      Canvas.lineTo context x (y + h_ length)
