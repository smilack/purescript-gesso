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
import Gesso.Interactions.Events
  ( Event
  , ClipboardEvent
  , FocusEvent
  , KeyboardEvent
  , TouchEvent
  , DragEvent
  , MouseEvent
  , WheelEvent
  )
import Halogen.HTML.Properties (IProp)

-- | This is the type of the `on` functions from `Gesso.Interactions.Events`.
-- | For example,
-- | `onClick :: forall r i. (MouseEvent -> i) -> IProp (onClick :: MouseEvent | r) i`
type EventProp event i = (event -> i) -> IProp HTMLcanvas i

-- | An event handler is a variant of an update function, which receives an
-- | event and produces an update function in response.
type Handler event localState = event -> App.UpdateFunction localState

-- | A combination of an event property, e.g. `onClick`, and an event handler.
data Interaction event localState =
  Interaction (forall i. EventProp event i) (Handler event localState)

-- | `Interactions` is a record containing arrays of interactions for each type
-- | of event that Canvas supports. It's used in
-- | [`Gesso.Canvas.Input`](Gesso.Canvas.html#t:Input) to add event handlers to
-- | a component.
type Interactions localState =
  { base :: Array (Interaction Event localState)
  , clipboard :: Array (Interaction ClipboardEvent localState)
  , focus :: Array (Interaction FocusEvent localState)
  , keyboard :: Array (Interaction KeyboardEvent localState)
  , touch :: Array (Interaction TouchEvent localState)
  , drag :: Array (Interaction DragEvent localState)
  , mouse :: Array (Interaction MouseEvent localState)
  , wheel :: Array (Interaction WheelEvent localState)
  }

-- | An default `Interactions` record containing no interactions. The
-- | attributes can be overridden individually instead of manually creating a
-- | complete but mostly empty record. For example:
-- |
-- | `Interactions.default { keyboard = [ a, b, c ] }`
default :: forall localState. Interactions localState
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

-- | Convert an `Interactions` record to an array of HTML properties. The return
-- | value of the `toCallback` parameter, `i`, is only known by the component
-- | and should be whatever `Action` type the component has, like `QueueUpdate`
-- | in Canvas.
toProps
  :: forall localState i
   . (App.UpdateFunction localState -> i)
  -> Interactions localState
  -> Array (IProp HTMLcanvas i)
toProps
  toCallback
  { base, clipboard, focus, keyboard, touch, drag, mouse, wheel } =
  map toProp base
    <> map toProp clipboard
    <> map toProp focus
    <> map toProp keyboard
    <> map toProp touch
    <> map toProp drag
    <> map toProp mouse
    <> map toProp wheel
  where
  toProp :: forall e. Interaction e localState -> IProp HTMLcanvas i
  toProp (Interaction onEvent handler) =
    onEvent $ toCallback <<< handler

-- | A useful example interaction that updates the mouse position on every
-- | `MouseMove` event, which works with all state types that are records
-- | containing at least a `mousePos :: Maybe Gesso.Dimensions.Point` field.
mousePosition
  :: forall moreState
   . Interaction MouseEvent { mousePos :: Maybe Dims.Point | moreState }
mousePosition =
  Interaction Events.onMouseMove getMousePos
  where
  getMousePos event _ _ state = pure $
    Just state { mousePos = Just $ Dims.fromMouseEvent event }
