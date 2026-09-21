module Main (main) where

import Prelude

import Control.Apply (lift2)
import Controls (initialState, State, handleInput, createControls)
import Data.Foldable (for_)
import Data.Int (round)
import Data.Maybe (maybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (error, throwError)
import Gesso (QuerySelector(..), runGessoAff, selectElement, awaitLoad)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Canvas (CanvasInput(..))
import Gesso.Canvas (component) as Gesso.Canvas
import Gesso.Geometry (Align, Alignment(..), Box, Point, PreserveAspectRatio(..), Rect, Scaler, Scalers, Position, mkReferenceFrame, mkReferenceFrameWithRatio, (*~>), (-~>), (|~>))
import Gesso.Geometry (compose) as Geometry
import Gesso.State (States)
import Gesso.Time (Delta)
import Graphics.Canvas (Context2D)
import Graphics.Canvas as C
import Halogen.VDom.Driver (runUI)
import Util (rangeNumber)

main :: Effect Unit
main = runGessoAff do
  awaitLoad
  container <- maybe err pure =<< selectElement (QuerySelector selector)
  app <- runUI Gesso.Canvas.component
    { name: "frames"
    , initialState
    , viewBox: { x: -2.1, y: -1.6, width: 4.2, height: 3.7 }
    , window: Fullscreen
    , behavior: defaultBehavior { render = render, input = handleInput }
    }
    container
  createControls \q -> app.query (CanvasInput q unit)
  where
  selector = "#frameExample"
  err = throwError $ error $ "Could not find " <> selector

faded :: Number
faded = 0.33

alCoords :: Alignment -> Number
alCoords = case _ of
  Min -> -1.0
  Mid -> 0.0
  Max -> 1.0

alignments :: Array Alignment
alignments = [ Min, Mid, Max ]

render :: Context2D -> Delta -> Scalers -> States State -> Effect Unit
render ctx _ { canvas } { current } = do
  C.clearRect ctx canvas.rect

  if anyGridShown then
    C.setGlobalAlpha ctx faded
  else
    C.setGlobalAlpha ctx 1.0

  drawTable ctx canvas
  drawCells ctx canvas current
  when current.showDrawingGrid drawDrawingGrid

  where
  anyGridShown =
    current.showDrawingGrid
      || current.showCellGrids
      || current.showExampleGrids

  neg2To2 = rangeNumber (-2.0) 2.0
  points = lift2 { x: _, y: _ } neg2To2 neg2To2

  drawDrawingGrid :: Effect Unit
  drawDrawingGrid = drawHighlighted ctx do
    drawUnitGrid ctx { x: -2.0, y: -2.0, width: 4.0, height: 4.0 } canvas
    labelPoints ctx 0.03 points canvas

drawHighlighted :: Context2D -> Effect Unit -> Effect Unit
drawHighlighted ctx drawing = do
  C.setGlobalAlpha ctx 1.0
  drawing
  C.setGlobalAlpha ctx faded

-- The k parameter is kind of arbitrary, it's just a size that looked decent for
-- each grid
labelPoints :: Context2D -> Number -> Array Point -> Scaler -> Effect Unit
labelPoints ctx k points scale = do
  C.setFillStyle ctx "#11d"
  C.setFont ctx "11pt Arial"
  C.setTextAlign ctx C.AlignLeft
  C.setTextBaseline ctx C.BaselineTop
  for_ points \{ x, y } -> do
    C.fillRect ctx $
      { x: x - k, y: y - k, width: 2.0 * k, height: 2.0 * k } *~> scale
    let
      label = "(" <> show (round x) <> ", " <> show (round y) <> ")"
    C.fillText ctx label (x + k -~> scale) (y + k |~> scale)

drawUnitGrid :: Context2D -> Rect -> Scaler -> Effect Unit
drawUnitGrid ctx rect scale = do
  C.setLineWidth ctx 1.0
  C.setStrokeStyle ctx "blue"

  let
    scaledRect = rect *~> scale

  for_ (rangeNumber rect.x $ rect.x + rect.width) \x -> C.strokePath ctx $
    vLine ctx { x: x -~> scale, y: scaledRect.y, y': scaledRect.y + scaledRect.height }

  for_ (rangeNumber rect.y $ rect.y + rect.height) \y -> C.strokePath ctx $
    hLine ctx { x: scaledRect.x, x': scaledRect.x + scaledRect.width, y: y |~> scale }

drawCells :: Context2D -> Scaler -> State -> Effect Unit
drawCells ctx canvas { showCellGrids, showExampleGrids } = do
  traverse_ drawExampleCell $ lift2 { x: _, y: _ } alignments alignments
  where
  drawExampleCell :: Align -> Effect Unit
  drawExampleCell align = do
    let
      inner = { x: 0.0, y: 0.0, width: 10.0, height: 10.0 }
      outer = { x: alCoords align.x + 0.1, y: alCoords align.y + 0.1, width: 0.8, height: 0.8 }
      exampleFrame = mkReferenceFrame { inner, outer }
      scale = Geometry.compose exampleFrame.outer canvas

    let
      rects =
        { landscape:
            { meet: { x: 0.0, y: 0.0, width: 4.0, height: 3.0 }
            , slice: { x: 0.0, y: 7.0, width: 4.0, height: 3.0 }
            }
        , portrait:
            { meet: { x: 6.0, y: 0.0, width: 3.0, height: 4.0 }
            , slice: { x: 6.0, y: 6.0, width: 3.0, height: 4.0 }
            }
        }

    drawSingleExample (Meet align) rects.landscape.meet scale
    drawSingleExample (Slice align) rects.landscape.slice scale
    drawSingleExample (Meet align) rects.portrait.meet scale
    drawSingleExample (Slice align) rects.portrait.slice scale

    when showCellGrids $ drawHighlighted ctx do
      drawUnitGrid ctx inner scale
      labelPoints ctx 0.2 (lift2 { x: _, y: _ } [ 0.0, 5.0, 10.0 ] [ 0.0, 5.0, 10.0 ]) scale

  drawSingleExample :: PreserveAspectRatio -> Rect -> Scaler -> Effect Unit
  drawSingleExample pAR outer scale = do
    let
      frame = mkReferenceFrameWithRatio pAR { outer, inner: { x: 0.0, y: 0.0, width: 1.0, height: 1.0 } }
      scale' = Geometry.compose frame.outer scale

    C.setLineWidth ctx 6.0
    C.setStrokeStyle ctx "black"
    C.strokeRect ctx $ outer *~> scale

    C.setLineWidth ctx 4.0
    C.setStrokeStyle ctx "#ee22ee"
    C.strokeRect ctx $ frame.inner.rect *~> scale'

    when showExampleGrids $ drawHighlighted ctx do
      drawUnitGrid ctx frame.inner.rect scale'
      labelPoints ctx 0.1 (lift2 { x: _, y: _ } [ 0.0, 1.0 ] [ 0.0, 1.0 ]) scale'

drawTable :: Context2D -> Scaler -> Effect Unit
drawTable ctx canvas = do
  drawRowBackgrounds
  let border = { left: -2.0, top: -1.5, right: 2.0, bottom: 2.0 } *~> canvas
  drawSubBorders border
  drawBorders border
  drawHeaders
  where
  drawRowBackgrounds :: Effect Unit
  drawRowBackgrounds = do
    C.setFillStyle ctx "#f5f5f5"
    for_ alignments \al -> do
      C.fillRect ctx $ { x: -1.5, y: alCoords al, width: 3.5, height: 0.5 } *~> canvas

  drawSubBorders :: Box -> Effect Unit
  drawSubBorders border = do
    C.setStrokeStyle ctx "gray"
    C.setLineWidth ctx 1.0
    C.strokePath ctx do
      let
        left = -1.5 -~> canvas
      vLine ctx
        { x: -1.5 -~> canvas
        , y: -1.0 |~> canvas
        , y': border.bottom
        }

      for_ alignments \al -> do
        hLine ctx { x: left, x': border.right, y: alCoords al + 0.5 |~> canvas }

  drawBorders :: Box -> Effect Unit
  drawBorders border = do
    C.setStrokeStyle ctx "black"
    C.setLineWidth ctx 2.0
    C.strokePath ctx $ for_ alignments \al -> do
      vLine ctx { x: alCoords al -~> canvas, y: border.top, y': border.bottom }
      hLine ctx { x: border.left, x': border.right, y: alCoords al |~> canvas }

  drawHeaders :: Effect Unit
  drawHeaders = do
    C.setFillStyle ctx "black"
    C.setFont ctx "16pt Arial"
    C.setTextAlign ctx C.AlignCenter
    C.setTextBaseline ctx C.BaselineMiddle

    let
      alignment =
        { x: -1.25 |~> canvas
        , y: -1.75 -~> canvas
        , meetOrSlice: -1.25 -~> canvas
        }

    for_ alignments \al -> do
      C.fillText ctx ("X-" <> alignStrs al) (alCoords al + 0.5 -~> canvas) alignment.x
      C.fillText ctx ("Y-" <> alignStrs al) alignment.y (alCoords al + 0.5 |~> canvas)
      C.fillText ctx "Meet" alignment.meetOrSlice (alCoords al + 0.25 |~> canvas)
      C.fillText ctx "Slice" alignment.meetOrSlice (alCoords al + 0.75 |~> canvas)

  alignStrs :: Alignment -> String
  alignStrs = case _ of
    Min -> "Min"
    Mid -> "Mid"
    Max -> "Max"

vLine :: Context2D -> { | Position Number (y' :: Number) } -> Effect Unit
vLine ctx { x, y, y' } = do
  C.moveTo ctx x y
  C.lineTo ctx x y'

hLine :: Context2D -> { | Position Number (x' :: Number) } -> Effect Unit
hLine ctx { x, x', y } = do
  C.moveTo ctx x y
  C.lineTo ctx x' y
