module Gesso.Example.PaintApp.Root where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (fromFoldable)
import Data.List (List(..), (:), tail, reverse, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Gesso.Example.PaintApp.ColorButton as CB
import Gesso.Example.PaintApp.Grid (CanvasIO)
import Gesso.Example.PaintApp.Grid as Grid
import Gesso.Canvas as GC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slots =
  ( colorButton :: CB.Slot Int
  | Grid.Slot ()
  )

data Action
  = ButtonClicked CB.Output
  | Undo
  | Redo
  | ToggleGrid
  | GotOutput (GC.CanvasOutput CanvasIO)

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component = H.mkComponent
  { initialState: const Grid.publicInitialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

render :: forall m. MonadAff m => CanvasIO -> H.ComponentHTML Action Slots m
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
      [ HH.slot CB._colorButton 0 CB.component (picker "black") ButtonClicked
      , HH.slot CB._colorButton 1 CB.component (picker "#888") ButtonClicked
      , HH.slot CB._colorButton 2 CB.component (picker "#ccc") ButtonClicked
      , HH.slot CB._colorButton 3 CB.component (picker "white") ButtonClicked
      ]

  picker = { selected: state.color, color: _ }

  drawing =
    HH.div [ style styles.canvas ]
      [ Grid.component GotOutput
      , HH.label [ style styles.label ]
          [ HH.input
              [ HP.type_ InputCheckbox
              , HE.onClick (const ToggleGrid)
              , HP.checked state.showGrid
              ]
          , HH.span_ [ HH.text "Show Grid" ]
          ]
      ]

  undoRedoHistory =
    HH.div
      [ style styles.history ]
      [ HH.button [ HE.onClick (const Undo), style styles.control ]
          [ HH.text "⟲ Undo" ]
      , HH.button [ HE.onClick (const Redo), style styles.control ]
          [ HH.text "⟳ Redo" ]
      , HH.ul
          [ style "list-style-type: none;" ]
          $ history styles.redo (reverse state.redo)
              <>
                [ HH.li [ style styles.place ]
                    [ HH.span [ style styles.line ] [] ]
                ]
              <> history "" state.pixels
      ]

  history sty pixels = fromFoldable $ map listItem pixels
    where
    listItem (Grid.Pixel { x, y, color }) =
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

send
  :: forall o m
   . MonadAff m
  => CanvasIO
  -> H.HalogenM CanvasIO Action Slots o m Unit
send state = do
  H.tell GC._gessoCanvas unit $ GC.CanvasInput $ toIO state
  pure unit

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM CanvasIO Action Slots o m Unit
handleAction = case _ of
  ToggleGrid ->
    (H.modify \s -> s { showGrid = not s.showGrid } :: CanvasIO) >>= send
  GotOutput (GC.CanvasOutput output') -> H.put output'
  ButtonClicked (CB.Clicked color') ->
    H.modify (_ { color = color' }) >>= send
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
        H.modify (_ { pixels = pixels', redo = redo' }) >>= send
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

          state' = state { pixels = pixels', redo = redo' }
        H.put state'
        send state'

toIO :: forall r. { | Grid.CanvasIO' r } -> CanvasIO
toIO { showGrid, color, pixels, redo } = { showGrid, color, pixels, redo }
