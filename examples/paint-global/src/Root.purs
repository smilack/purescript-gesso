module Root where

import Prelude
import ColorButton as CB
import Data.Array (range)
import Data.Int (floor, toNumber)
import Data.List (List(..), (:), tail, reverse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sequence_, traverse_)
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

type AppState
  = { mouseCell :: Maybe { x :: Int, y :: Int }
    , clicked :: Maybe { x :: Number, y :: Number }
    , showGrid :: Boolean
    , color :: String
    , pixels :: List Pixel
    }

newtype Pixel
  = Pixel { x :: Int, y :: Int, color :: String }

type Slots
  = ( colorButton :: CB.Slot Int
    , gessoCanvas :: GC.Slot Unit
    )

data Action
  = ButtonClicked CB.Output
  | Undo
  | Initialize
  | StateUpdated AppState

initialState :: forall i. i -> AppState
initialState _ =
  { mouseCell: Nothing
  , clicked: Nothing
  , showGrid: true
  , color: "black"
  , pixels: Nil
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
    []
    [ HH.slot CB._colorButton 0 CB.component "black" (Just <<< ButtonClicked)
    , HH.slot CB._colorButton 1 CB.component "#888" (Just <<< ButtonClicked)
    , HH.slot CB._colorButton 2 CB.component "#ccc" (Just <<< ButtonClicked)
    , HH.slot CB._colorButton 3 CB.component "white" (Just <<< ButtonClicked)
    , HH.button [ HE.onClick (Just <<< const Undo) ] [ HH.text "undo" ]
    , HH.div []
        [ HH.slot GC._gessoCanvas unit GC.component (canvasInput state) absurd ]
    ]

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
  StateUpdated appState' -> H.put appState'
  ButtonClicked (CB.Clicked color) -> do
    appState <- GM.getState
    GM.putState $ appState { color = color }
  Undo -> do
    appState <- GM.getState
    let
      pixels' = fromMaybe Nil $ tail appState.pixels
    GM.putState $ appState { pixels = pixels' }

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
  , interactions: GInt.default { mouse = [ highlightCell, clearHighlight, mouseDown ] }
  }

highlightCell ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
highlightCell = GInt.mkInteraction GEv.onMouseMove getMousePos
  where
  getMousePos event scaler = (_ { mouseCell = Just $ toXY event scaler })

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
clearHighlight = GInt.mkInteraction GEv.onMouseOut (\_ _ s -> s { mouseCell = Nothing })

mouseDown ::
  forall i.
  GInt.Interaction GEv.MouseEvent AppState i
mouseDown = GInt.mkInteraction GEv.onMouseDown getMousePos
  where
  getMousePos event scaler state =
    let
      { x, y } = toXY event scaler

      p = Pixel { x, y, color: state.color }
    in
      state { pixels = p : state.pixels }

renderApp :: AppState -> GTime.Delta -> GDim.Scaler -> Canvas.Context2D -> Effect Unit
renderApp { clicked, mouseCell, showGrid, color, pixels } _ { x_, y_, w_, h_, screen, toVb } context = do
  clearBackground
  when showGrid drawGrid
  drawImage
  traverse_ drawCursor mouseCell
  where
  clearBackground :: Effect Unit
  clearBackground = do
    Canvas.setFillStyle context "white"
    Canvas.fillRect context screen

  drawGrid :: Effect Unit
  drawGrid = do
    Canvas.setLineWidth context $ w_ 0.05
    Canvas.setStrokeStyle context "#888"
    Canvas.strokeRect context screen
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
  drawCursor { x, y } = do
    Canvas.setFillStyle context color
    Canvas.fillRect context { x: x_ (toNumber x), y: y_ (toNumber y), width: w_ 1.0, height: h_ 1.0 }

  drawImage :: Effect Unit
  drawImage = sequence_ $ map drawPixel $ reverse pixels

  drawPixel :: Pixel -> Effect Unit
  drawPixel (Pixel { x, y, color: c }) = do
    Canvas.setFillStyle context c
    Canvas.fillRect context { x: x_ $ toNumber x, y: y_ $ toNumber y, width: w_ 1.0, height: h_ 1.0 }
