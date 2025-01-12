-- | Interactions are a wrapper for HTML events that allow specifying event
-- | types and handlers which are attached to the drawing screen when it
-- | renders. Interactions are an input to the Canvas component, and will
-- | typically be specified as an update of the [`default`](#v:default) record,
-- | similar to `mkEval` and `defaultEval` in Halogen.
-- |
-- | The [`Interactions`](#t:Interactions) type is a record of arrays of
-- | interactions for each event type.
-- |
-- | [`mousePosition`](#v:mousePosition) is an example of how an interaction can
-- | be written.
module Gesso.Interactions
  ( EventProp
  , Handler
  , Interaction(..)
  , InteractionList
  , Interactions
  , default
  , toProps
  , mousePosition
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed (HTMLcanvas)
import Gesso.Application (UpdateFunction) as App
import Gesso.Dimensions as Dims
import Gesso.Interactions.Events as Events
import Gesso.Interactions.Events (Event, ClipboardEvent, FocusEvent, KeyboardEvent, TouchEvent, DragEvent, MouseEvent, WheelEvent)
import Halogen.HTML.Properties (IProp)

-- | This is the type of the `on` functions from `Gesso.Interactions.Events`.
-- | For example,
-- | `onClick :: forall r i. (MouseEvent -> i) -> IProp (onClick :: MouseEvent | r) i`
type EventProp event i = (event -> i) -> IProp HTMLcanvas i

-- | Alias for an event handler, which receives an event and returns an
-- | `UpdateFunction`. An `UpdateFunction` receives a time delta (`Gesso.Time`),
-- | a coordinate scaler (`Gesso.Dimensions`), and the current state, and may
-- | return an updated state if the state should change in response to the
-- | event.
type Handler event localState = event -> App.UpdateFunction localState

-- | An `Interaction` is a combination of an event property
-- | ([`EventProp`](#t:EventProp) e.g., `onClick`) and an event handler.
data Interaction event localState i =
  Interaction (EventProp event i) (Handler event localState)

-- | Alias for an array of [`Interaction`s](#t:Interaction)
type InteractionList event localState i = Array (Interaction event localState i)

-- | `Interactions` is a record containing arrays of interactions for each type
-- | of event that Canvas supports.
type Interactions localState i =
  { base :: InteractionList Event localState i
  , clipboard :: InteractionList ClipboardEvent localState i
  , focus :: InteractionList FocusEvent localState i
  , keyboard :: InteractionList KeyboardEvent localState i
  , touch :: InteractionList TouchEvent localState i
  , drag :: InteractionList DragEvent localState i
  , mouse :: InteractionList MouseEvent localState i
  , wheel :: InteractionList WheelEvent localState i
  }

-- | Convert an [`Interactions`](#t:Interactions) record to an array of HTML
-- | properties. `i` - the return value of the `toCallback` parameter - should
-- | be whatever `Action` type the component has, like `QueueUpdate` in Canvas.
toProps
  :: forall localState i
   . (App.UpdateFunction localState -> i)
  -> Interactions localState i
  -> Array (IProp HTMLcanvas i)
toProps toCallback { base, clipboard, focus, keyboard, touch, drag, mouse, wheel } =
  map toProp base
    <> map toProp clipboard
    <> map toProp focus
    <> map toProp keyboard
    <> map toProp touch
    <> map toProp drag
    <> map toProp mouse
    <> map toProp wheel
  where
  toProp :: forall e. Interaction e localState i -> IProp HTMLcanvas i
  toProp (Interaction onEvent handler) =
    onEvent $ toCallback <<< handler

-- | An [`Interactions`](#t:Interactions) record containing no interactions. The
-- | attributes can be overridden individually instead of manually creating a
-- | complete but mostly empty record. For example:
-- |
-- | `default { keyboard = [ a, b, c ] }`
default :: forall localState i. Interactions localState i
default =
  { base: []
  , clipboard: []
  , focus: []
  , keyboard: []
  , touch: []
  , drag: []
  , mouse: []
  , wheel: []
  }

-- | A useful example [`Interaction`](#t:Interaction) that updates the mouse
-- | position on every `MouseMove` event, which works with all state types that
-- | are records containing at least a
-- | `mousePos :: Maybe Gesso.Dimensions.Point` field.
mousePosition
  :: forall moreState i
   . Interaction MouseEvent { mousePos :: Maybe Dims.Point | moreState } i
mousePosition =
  Interaction Events.onMouseMove getMousePos
  where
  getMousePos event _ _ state = pure $
    Just state { mousePos = Just $ Dims.fromMouseEvent event }
