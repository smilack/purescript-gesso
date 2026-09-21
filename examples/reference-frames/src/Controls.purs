module Gesso.Example.ReferenceFrames.Controls
  ( InputType
  , State
  , createControls
  , handleInput
  , initialState
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Gesso (QuerySelector(..), selectElement)
import Gesso.Application (InputReceiver)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLElement (toEventTarget)

type State =
  { showDrawingGrid :: Boolean
  , showCellGrids :: Boolean
  , showExampleGrids :: Boolean
  }

initialState :: State
initialState =
  { showDrawingGrid: false
  , showCellGrids: false
  , showExampleGrids: false
  }

data InputType
  = DrawingGrid
  | CellGrids
  | ExampleGrids

derive instance Eq InputType

createControls :: (InputType -> Aff (Maybe Unit)) -> Aff Unit
createControls query = do
  createControl (query DrawingGrid) "#drawingGrid"
  createControl (query CellGrids) "#cellGrids"
  createControl (query ExampleGrids) "#exampleGrids"

createControl :: Aff (Maybe Unit) -> String -> Aff Unit
createControl query sel = do
  mElement <- selectElement (QuerySelector sel)
  liftEffect case mElement of
    Nothing -> throwError $ error $ "Could not find " <> sel
    Just element -> do
      listener <- eventListener (handleEvent query)
      addEventListener (EventType "input") listener true (toEventTarget element)

handleEvent :: Aff (Maybe Unit) -> Event -> Effect Unit
handleEvent query _ = launchAff_ $ void query

handleInput :: InputReceiver State InputType
handleInput inType _ _ state = pure $ Just
  { showDrawingGrid: state.showDrawingGrid /= (inType == DrawingGrid)
  , showCellGrids: state.showCellGrids /= (inType == CellGrids)
  , showExampleGrids: state.showExampleGrids /= (inType == ExampleGrids)
  }
