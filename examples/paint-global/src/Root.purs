module Example.PaintGlobal.Root where

import Prelude
import Example.PaintGlobal.ColorButton as CB
import Data.Array (range, fromFoldable)
import Data.Foldable (sequence_, traverse_, length)
import Data.Int (floor, toNumber)
import Data.List (List(..), (:), tail, reverse, head)
import Data.Maybe (Maybe(..), fromMaybe)
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Gesso.Application as GApp
import Gesso.AspectRatio as GAR
import Gesso.Canvas as GC
import Gesso.Dimensions as GDim
import Gesso.GessoM (class ManageState)
import Gesso.GessoM as GM
import Gesso.Interactions as GInt
import Gesso.Interactions.Events as GEv
import Gesso.Time as GTime
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (merge) as Record

type CanvasState
  = {
    | CanvasIO'
      ( mouseCell :: Maybe { x :: Int, y :: Int }
      , clicked :: Maybe { x :: Number, y :: Number }
      , mouseDown :: Boolean
      )
    }

type RootState
  = CanvasIO

type CanvasIO
  = { | CanvasIO' () }

-- These are all the things that Root needs to know. If Root is aware
--   of mouseCell, it changes the global state too often, causing lag.
type CanvasIO' r
  = ( showGrid :: Boolean
    , color :: String
    , pixels :: List Pixel
    , redo :: List Pixel
    | r
    )

newtype Pixel
  = Pixel { x :: Int, y :: Int, color :: String }

derive instance eqPixel :: Eq Pixel

type Slots
  = ( colorButton :: CB.Slot Int
    , gessoCanvas :: forall i o. GC.Slot i o Unit
    )

data Action
  = ButtonClicked CB.Output
  | Undo
  | Redo
  | ToggleGrid
  | GotGlobal CanvasIO
  | Initialize

initialState :: forall i. i -> RootState
initialState _ =
  { showGrid: true
  , color: "black"
  , pixels: Nil
  , redo: Nil
  }

canvasInitialState :: CanvasState
canvasInitialState =
  Record.merge (initialState unit)
    { mouseCell: Nothing
    , clicked: Nothing
    , mouseDown: false
    }

component ::
  forall q i o m.
  MonadAff m =>
  ManageState m CanvasIO =>
  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

render :: forall m. MonadAff m => ManageState m CanvasIO => RootState -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ style styles.root ]
    [ colorPicker
    , drawing
    , undoRedoHistory
    ]
  where
  colorPicker =
    HH.div [ style styles.colorPicker ]
      [ HH.slot CB._colorButton 0 CB.component (picker "black") (Just <<< ButtonClicked)
      , HH.slot CB._colorButton 1 CB.component (picker "#888") (Just <<< ButtonClicked)
      , HH.slot CB._colorButton 2 CB.component (picker "#ccc") (Just <<< ButtonClicked)
      , HH.slot CB._colorButton 3 CB.component (picker "white") (Just <<< ButtonClicked)
      ]

  picker = { selected: state.color, color: _ }

  drawing =
    HH.div [ style styles.canvas ]
      [ HH.slot GC._gessoCanvas unit GC.component (canvasInput canvasInitialState) (const Nothing)
      , HH.label [ style styles.label ]
          [ HH.input [ HP.type_ InputCheckbox, HE.onClick (Just <<< const ToggleGrid), HP.checked state.showGrid ]
          , HH.span_ [ HH.text "Show Grid" ]
          ]
      ]

  undoRedoHistory =
    HH.div
      [ style styles.history ]
      [ HH.button [ HE.onClick (Just <<< const Undo), style styles.control ] [ HH.text "⟲ Undo" ]
      , HH.button [ HE.onClick (Just <<< const Redo), style styles.control ] [ HH.text "⟳ Redo" ]
      , HH.ul
          [ style "list-style-type: none;" ]
          $ history styles.redo (reverse state.redo)
          <> [ HH.li [ style styles.place ] [ HH.span [ style styles.line ] [] ] ]
          <> history "" state.pixels
      ]

  history sty pixels = fromFoldable $ map listItem pixels
    where
    listItem (Pixel { x, y, color }) =
      HH.li []
        [ pixelBlock color
        , HH.span [ style sty ]
            [ HH.text $ " (" <> show x <> ", " <> show y <> ")" ]
        ]

  pixelBlock color =
    HH.span
      [ style
          $ "display: inline-block;"
          <> "width: 10px;"
          <> "height: 10px;"
          <> "border: 1px black solid;"
          <> ("background-color: " <> color)
      ]
      []

  style :: forall r i. String -> HP.IProp r i
  style = HP.attr (HH.AttrName "style")

  styles =
    { root: "display: flex; font-family: sans-serif; justify-content: center;"
    , colorPicker: "display: flex; flex-direction: column;"
    , canvas: "display: flex; flex-direction: column; margin: 6px 0; align-items: center;"
    , label: "display: block; margin: 6px 0; font-size: 24px; cursor: pointer;"
    , history: "margin: 6px 12px; max-height: 600px; overflow: hidden scroll;"
    , control: "font-size: 24px;"
    , redo: "opacity: 0.33;"
    , place: "padding-left: 3px; list-style-type: '⮞';"
    , line: "display: inline-block; width: 100%; height: 2px; background-color: black; vertical-align: middle; margin-bottom: 2px;"
    }

handleAction ::
  forall o m.
  MonadAff m =>
  ManageState m CanvasIO =>
  Action -> H.HalogenM RootState Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    eventSource <- GM.getEventSource
    _ <- H.subscribe $ GotGlobal <$> eventSource
    pure unit
  GotGlobal state' -> H.modify_ $ convertState state'
  ToggleGrid -> GM.modifyState_ \s -> s { showGrid = not s.showGrid }
  ButtonClicked (CB.Clicked color') -> GM.modifyState_ (_ { color = color' })
  Undo -> do
    state <- H.get
    let
      step = head state.pixels

      pixels' = fromMaybe Nil $ tail state.pixels
    case step of
      Nothing -> pure unit
      Just pixel -> do
        let
          redo' = pixel : state.redo
        GM.modifyState_ (_ { pixels = pixels', redo = redo' })
  Redo -> do
    state <- H.get
    let
      step = head state.redo

      redo' = fromMaybe Nil $ tail state.redo
    case step of
      Nothing -> pure unit
      Just pixel -> do
        let
          pixels' = pixel : state.pixels
        GM.modifyState_ (_ { pixels = pixels', redo = redo' })

canvasInput :: forall i o. CanvasState -> GC.Input CanvasState CanvasIO i o
canvasInput localState =
  { name: "canvas"
  , localState
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fixed $ GDim.fromWidthAndHeight { width: 600.0, height: 600.0 }
            , render = Just $ GApp.onChange renderApp
            , output = GApp.globalState
            , global { toLocal = convertState, fromLocal = convertState }
            }
  , viewBox:
      GDim.fromPointAndSize
        GDim.origin
        (GDim.fromWidthAndRatio { width: 32.0, aspectRatio: GAR.w1h1 })
  , interactions: GInt.default { mouse = [ highlightCell, clearHighlight, mouseDown, mouseUp ] }
  }

toIO :: forall r. { | CanvasIO' r } -> CanvasIO
toIO { showGrid, color, pixels, redo } = { showGrid, color, pixels, redo }

convertState ::
  forall r s.
  { | CanvasIO' r } ->
  { | CanvasIO' s } ->
  { | CanvasIO' s }
convertState { showGrid, color, pixels, redo } = _ { showGrid = showGrid, color = color, pixels = pixels, redo = redo }

extractOutput :: CanvasState -> CanvasState -> Maybe CanvasIO
extractOutput state state'@{ showGrid, color, pixels, redo } =
  if (state.showGrid /= showGrid)
    || (state.color /= color)
    || (length state.pixels /= (length pixels :: Int))
    || (length state.redo /= (length redo :: Int)) then
    Just { showGrid, color, pixels, redo }
  else
    Nothing

highlightCell ::
  forall i.
  GInt.Interaction GEv.MouseEvent CanvasState i
highlightCell = GInt.mkInteraction GEv.onMouseMove getMousePos
  where
  getMousePos event _ scaler state =
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

clearHighlight ::
  forall i.
  GInt.Interaction GEv.MouseEvent CanvasState i
clearHighlight = GInt.mkInteraction GEv.onMouseOut (\_ _ _ s -> Just s { mouseCell = Nothing })

mouseDown ::
  forall i.
  GInt.Interaction GEv.MouseEvent CanvasState i
mouseDown = GInt.mkInteraction GEv.onMouseDown startDrawing
  where
  startDrawing event _ scaler state =
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      Just state { pixels = p : state.pixels, redo = Nil, mouseDown = true }

mouseUp ::
  forall i.
  GInt.Interaction GEv.MouseEvent CanvasState i
mouseUp = GInt.mkInteraction GEv.onMouseUp (\_ _ _ s -> Just s { mouseDown = false })

renderApp :: CanvasState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
renderApp { clicked, mouseCell, showGrid, color, pixels } _ scale context = do
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
