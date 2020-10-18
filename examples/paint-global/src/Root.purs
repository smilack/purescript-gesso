module Root where

import Prelude
import ColorButton as CB
import Data.Array (range, fromFoldable)
import Data.Int (floor, toNumber)
import Data.List (List(..), (:), tail, reverse, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sequence_, traverse_)
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

type AppState
  = { mouseCell :: Maybe { x :: Int, y :: Int }
    , clicked :: Maybe { x :: Number, y :: Number }
    , showGrid :: Boolean
    , color :: String
    , pixels :: List Pixel
    , redo :: List Pixel
    , mouseDown :: Boolean
    }

newtype Pixel
  = Pixel { x :: Int, y :: Int, color :: String }

derive instance eqPixel :: Eq Pixel

type Slots
  = ( colorButton :: CB.Slot Int
    , gessoCanvas :: GC.Slot Unit
    )

data Action
  = ButtonClicked CB.Output
  | Undo
  | Redo
  | Initialize
  | StateUpdated AppState
  | ToggleGrid

initialState :: forall i. i -> AppState
initialState _ =
  { mouseCell: Nothing
  , clicked: Nothing
  , showGrid: true
  , color: "black"
  , pixels: Nil
  , redo: Nil
  , mouseDown: false
  }

component ::
  forall q i o m.
  MonadAff m =>
  ManageState m AppState =>
  H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

render :: forall m. MonadAff m => ManageState m AppState => AppState -> H.ComponentHTML Action Slots m
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
      [ HH.slot GC._gessoCanvas unit GC.component (canvasInput state) absurd
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
  forall s o m.
  MonadAff m =>
  ManageState m AppState =>
  Action -> H.HalogenM AppState Action s o m Unit
handleAction = case _ of
  Initialize -> do
    stateEventSource <- GM.getEventSource
    _ <- H.subscribe $ StateUpdated <$> stateEventSource
    pure unit
  ToggleGrid -> do
    appState <- GM.getState
    GM.putState $ appState { showGrid = not appState.showGrid }
  StateUpdated appState' -> H.put appState'
  ButtonClicked (CB.Clicked color) -> do
    appState <- GM.getState
    GM.putState $ appState { color = color }
  Undo -> do
    appState <- GM.getState
    let
      step = head appState.pixels

      pixels' = fromMaybe Nil $ tail appState.pixels

      appState' = appState { pixels = pixels' }
    case step of
      Nothing -> GM.putState appState'
      Just pixel -> GM.putState $ appState' { redo = pixel : appState.redo }
  Redo -> do
    appState <- GM.getState
    let
      step = head appState.redo

      redo' = fromMaybe Nil $ tail appState.redo
    case step of
      Nothing -> pure unit
      Just pixel -> do
        let
          pixels' = pixel : appState.pixels
        GM.putState $ appState { pixels = pixels', redo = redo' }

canvasInput :: AppState -> GC.Input AppState
canvasInput appState =
  { name: "canvas"
  , appState
  , app:
      GApp.mkApplication
        $ GApp.defaultApp
            { window = GApp.fixed $ GDim.fromWidthAndHeight { width: 600.0, height: 600.0 }
            , render = Just $ GApp.onChange renderApp
            }
  , viewBox:
      GDim.fromPointAndSize
        GDim.origin
        (GDim.fromWidthAndRatio { width: 32.0, aspectRatio: GAR.w1h1 })
  , interactions: GInt.default { mouse = [ highlightCell, clearHighlight, mouseDown, mouseUp ] }
  }

highlightCell ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
highlightCell = GInt.mkInteraction GEv.onMouseMove getMousePos
  where
  getMousePos event scaler state =
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
      else
        if state.mouseCell == Just { x, y } then
          Nothing
        else
          Just state { mouseCell = Just { x, y } }

toXY :: GEv.MouseEvent -> GDim.Scaler -> { x :: Int, y :: Int }
toXY event { toVb } =
  let
    point = GDim.fromMouseEvent event

    { x, y } = { x: GDim.getX point, y: GDim.getY point }

    x' = floor $ toVb.x' x

    y' = floor $ toVb.y' y
  in
    { x: x', y: y' }

clearHighlight ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
clearHighlight = GInt.mkInteraction GEv.onMouseOut (\_ _ s -> Just s { mouseCell = Nothing })

mouseDown ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
mouseDown = GInt.mkInteraction GEv.onMouseDown startDrawing
  where
  startDrawing event scaler state =
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      Just state { pixels = p : state.pixels, redo = Nil, mouseDown = true }

mouseUp ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
mouseUp = GInt.mkInteraction GEv.onMouseUp (\_ _ s -> Just s { mouseDown = false })

renderApp :: AppState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
renderApp { clicked, mouseCell, showGrid, color, pixels } _ { x_, y_, w_, h_, screen, toVb } context = do
  clearBackground
  drawOutline
  when showGrid drawGrid
  drawImage
  traverse_ drawCursor mouseCell
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context screen

  drawOutline :: Effect Unit
  drawOutline = do
    Canvas.setLineWidth context $ w_ 0.05
    Canvas.setStrokeStyle context "#888"
    Canvas.strokeRect context screen

  drawGrid :: Effect Unit
  drawGrid = do
    Canvas.setStrokeStyle context "#ccc"
    sequence_ $ map drawGridLine $ range 1 31

  drawGridLine :: Int -> Effect Unit
  drawGridLine i = do
    Canvas.strokePath context do
      Canvas.moveTo context (x_ n) (y_ 0.0)
      Canvas.lineTo context (x_ n) (y_ 32.0)
      Canvas.moveTo context (x_ 0.0) (y_ n)
      Canvas.lineTo context (x_ 32.0) (y_ n)
    where
    n = toNumber i

  drawCursor :: { x :: Int, y :: Int } -> Effect Unit
  drawCursor { x, y } = drawPixel $ Pixel { x, y, color }

  drawImage :: Effect Unit
  drawImage = sequence_ $ map drawPixel $ reverse pixels

  drawPixel :: Pixel -> Effect Unit
  drawPixel (Pixel { x, y, color: c }) = do
    Canvas.setFillStyle context c
    Canvas.fillRect context { x: x_ $ toNumber x, y: y_ $ toNumber y, width: w_ 1.0, height: h_ 1.0 }
