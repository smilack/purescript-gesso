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
import Gesso.AspectRatio as GAR
import Gesso.Canvas as GC
import Gesso.Dimensions as GDim
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
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
        { window = GApp.Fixed $ GDim.fromWidthAndHeight { width: 600.0, height: 600.0 }
        , render = renderApp
        , output = extractOutput
        , input = convertState
        }
  , viewBox:
      GDim.fromPointAndSize
        GDim.origin
        (GDim.fromWidthAndRatio { width: 32.0, aspectRatio: GAR.w1h1 })
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

extractOutput :: GTime.Delta -> GDim.Scaler -> GLerp.Versions CanvasState -> Effect (Maybe CanvasIO)
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

highlightCell :: GInt.Interaction GEv.MouseEvent CanvasState
highlightCell = GInt.Interaction GEv.onMouseMove getMousePos
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

toXY :: GEv.MouseEvent -> GDim.Scaler -> { x :: Int, y :: Int }
toXY event scale =
  let
    point = scale.point.toVb (GDim.fromMouseEvent event)

    x = floor $ GDim.getX point

    y = floor $ GDim.getY point
  in
    { x, y }

clearHighlight :: GInt.Interaction GEv.MouseEvent CanvasState
clearHighlight = GInt.Interaction GEv.onMouseOut (\_ _ _ s -> pure $ Just s { mouseCell = Nothing })

mouseDown :: GInt.Interaction GEv.MouseEvent CanvasState
mouseDown = GInt.Interaction GEv.onMouseDown startDrawing
  where
  startDrawing event _ scaler state = pure $
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      Just state { pixels = p : state.pixels, redo = Nil, mouseDown = true }

mouseUp :: GInt.Interaction GEv.MouseEvent CanvasState
mouseUp = GInt.Interaction GEv.onMouseUp (\_ _ _ s -> pure $ Just s { mouseDown = false })

renderApp :: Canvas.Context2D -> GTime.Delta -> GDim.Scaler -> GLerp.Lerp CanvasState -> Effect Unit
renderApp context _ scale { new: { mouseCell, showGrid, color, pixels } } = do
  clearBackground
  drawOutline
  when showGrid drawGrid
  drawImage
  traverse_ drawCursor mouseCell
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context (scale.toRectangle scale.screen)

  drawOutline :: Effect Unit
  drawOutline = do
    Canvas.setLineWidth context $ scale.width.toCr 0.05
    Canvas.setStrokeStyle context "#888"
    Canvas.strokeRect context (scale.toRectangle scale.screen)

  drawGrid :: Effect Unit
  drawGrid = do
    Canvas.setStrokeStyle context "#ccc"
    sequence_ $ map drawGridLine $ range 1 31

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (scale.x.toCr n) (scale.y.toCr 0.0)
      Canvas.lineTo context (scale.x.toCr n) (scale.y.toCr 32.0)
      Canvas.moveTo context (scale.x.toCr 0.0) (scale.y.toCr n)
      Canvas.lineTo context (scale.x.toCr 32.0) (scale.y.toCr n)
    where
    n = toNumber i

  drawCursor :: { x :: Int, y :: Int } -> Effect Unit
  drawCursor { x, y } = drawPixel $ Pixel { x, y, color }

  drawImage :: Effect Unit
  drawImage = sequence_ $ map drawPixel $ reverse pixels

  drawPixel :: Pixel -> Effect Unit
  drawPixel (Pixel { x, y, color: c }) = do
    Canvas.setFillStyle context c
    Canvas.fillRect context { x: scale.x.toCr $ toNumber x, y: scale.y.toCr $ toNumber y, width: scale.width.toCr 1.0, height: scale.height.toCr 1.0 }
