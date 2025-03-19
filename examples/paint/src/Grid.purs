module Example.Paint.Grid
  ( CanvasIO
  , CanvasIO'
  , Pixel(..)
  , Slot
  , component
  , publicInitialState
  ) where

import Prelude

import Data.Array (range)
import Data.Foldable (sequence_, traverse_, length)
import Data.Int (floor, toNumber)
import Data.List (List(..), (:), reverse, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as GApp
import Gesso.Canvas as GC
import Gesso.Geometry as GGeo
import Gesso.Interactions as GInt
import Gesso.Geometry ((>@), (^@), (~>@), (-@))
import Gesso.Time as GTime
import Gesso.Util.Lerp as GLerp
import Graphics.Canvas as Canvas
import Halogen.HTML as HH
import Record (merge) as Record

type CanvasIO = { | CanvasIO' () }

-- These are all the things that Root needs to know. If Root is aware
--   of mouseCell, it changes the global state too often, causing lag.
type CanvasIO' r =
  ( showGrid :: Boolean
  , color :: String
  , pixels :: List Pixel
  , redo :: List Pixel
  | r
  )

type CanvasState =
  {
  | CanvasIO'
      ( mouseCell :: Maybe { x :: Int, y :: Int }
      , clicked :: Maybe { x :: Number, y :: Number }
      , mouseDown :: Boolean
      )
  }

newtype Pixel = Pixel { x :: Int, y :: Int, color :: String }

derive instance eqPixel :: Eq Pixel

type Slot s = (gessoCanvas :: GC.Slot CanvasIO CanvasIO Unit | s)

component
  :: forall action slots m
   . MonadAff m
  => (GC.Output CanvasIO -> action)
  -> HH.ComponentHTML action (Slot slots) m
component action =
  HH.slot GC._gessoCanvas unit GC.component canvasInput action

publicInitialState :: CanvasIO
publicInitialState =
  { showGrid: true
  , color: "black"
  , pixels: Nil
  , redo: Nil
  }

localState :: CanvasState
localState =
  Record.merge publicInitialState
    { mouseCell: Nothing
    , clicked: Nothing
    , mouseDown: false
    }

canvasInput :: GC.Input CanvasState CanvasIO CanvasIO
canvasInput =
  { name: "canvas"
  , localState
  , app:
      GApp.defaultApp
        { window = GApp.Fixed { width: 600.0, height: 600.0 }
        , render = renderApp
        , output = extractOutput
        , input = convertState
        }
  , viewBox: { x: 0.0, y: 0.0, width: 32.0, height: 32.0 }
  , interactions: GInt.default { mouse = [ highlightCell, clearHighlight, mouseDown, mouseUp ] }
  }

convertState
  :: forall delta scaler r s
   . { | CanvasIO' r }
  -> delta
  -> scaler
  -> { | CanvasIO' s }
  -> Effect (Maybe { | CanvasIO' s })
convertState { showGrid, color, pixels, redo } _ _ = pure
  <<< Just
  <<< _ { showGrid = showGrid, color = color, pixels = pixels, redo = redo }

extractOutput :: GTime.Delta -> GGeo.Scalers -> GLerp.Versions CanvasState -> Effect (Maybe CanvasIO)
extractOutput _ _ { old, new: { showGrid, color, pixels, redo } } =
  pure $
    if
      (old.showGrid /= showGrid)
        || (old.color /= color)
        || (length old.pixels /= (length pixels :: Int))
        || (length old.redo /= (length redo :: Int)) then
      Just { showGrid, color, pixels, redo }
    else
      Nothing

highlightCell :: GInt.MouseInteraction CanvasState
highlightCell = GInt.onMouseMove getMousePos
  where
  getMousePos event _ scaler state = pure $
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      if state.mouseDown then case head state.pixels of
        Nothing -> Just state { mouseCell = Just { x, y }, pixels = p : state.pixels }
        Just pixel ->
          if p == pixel then
            if state.mouseCell == Just { x, y } then
              Nothing
            else
              Just state { mouseCell = Just { x, y } }
          else
            Just state { mouseCell = Just { x, y }, pixels = p : state.pixels }
      else if state.mouseCell == Just { x, y } then
        Nothing
      else
        Just state { mouseCell = Just { x, y } }

toXY :: GInt.MouseEvent -> GGeo.Scalers -> { x :: Int, y :: Int }
toXY event { drawing } =
  let
    point = GGeo.fromMouseEvent event

    x = floor $ point.x >@ drawing

    y = floor $ point.y ^@ drawing
  in
    { x, y }

clearHighlight :: GInt.MouseInteraction CanvasState
clearHighlight = GInt.onMouseOut (\_ _ _ s -> pure $ Just s { mouseCell = Nothing })

mouseDown :: GInt.MouseInteraction CanvasState
mouseDown = GInt.onMouseDown startDrawing
  where
  startDrawing event _ scaler state = pure $
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      Just state { pixels = p : state.pixels, redo = Nil, mouseDown = true }

mouseUp :: GInt.MouseInteraction CanvasState
mouseUp = GInt.onMouseUp (\_ _ _ s -> pure $ Just s { mouseDown = false })

renderApp :: Canvas.Context2D -> GTime.Delta -> GGeo.Scalers -> GLerp.Lerp CanvasState -> Effect Unit
renderApp context _ { canvas } { new: { mouseCell, showGrid, color, pixels } } = do
  clearBackground
  drawOutline
  when showGrid drawGrid
  drawImage
  traverse_ drawCursor mouseCell
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context canvas.rect

  drawOutline :: Effect Unit
  drawOutline = do
    Canvas.setLineWidth context $ 0.05 -@ canvas
    Canvas.setStrokeStyle context "#888"
    Canvas.strokeRect context canvas.rect

  drawGrid :: Effect Unit
  drawGrid = do
    Canvas.setStrokeStyle context "#ccc"
    sequence_ $ map drawGridLine $ range 1 31

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (n >@ canvas) (0.0 ^@ canvas)
      Canvas.lineTo context (n >@ canvas) (32.0 ^@ canvas)
      Canvas.moveTo context (0.0 >@ canvas) (n ^@ canvas)
      Canvas.lineTo context (32.0 >@ canvas) (n ^@ canvas)
    where
    n = toNumber i

  drawCursor :: { x :: Int, y :: Int } -> Effect Unit
  drawCursor { x, y } = drawPixel $ Pixel { x, y, color }

  drawImage :: Effect Unit
  drawImage = sequence_ $ map drawPixel $ reverse pixels

  drawPixel :: Pixel -> Effect Unit
  drawPixel (Pixel { x, y, color: c }) = do
    Canvas.setFillStyle context c
    Canvas.fillRect context $
      { x: toNumber x, y: toNumber y, width: 1.0, height: 1.0 } ~>@ canvas
