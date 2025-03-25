-- | Definitions of the Interaction type as well as event handlers and event
-- | listener HTML properties.
module Gesso.Interactions.Internal
  ( EventProp
  , Handler
  , Interaction(..)
  , Interactions
  , toProps
  , module Exports
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLcanvas)
import Gesso.Application.Behavior (UpdateFunction) as App
import Halogen.HTML.Properties (IProp)
import Web.Clipboard.ClipboardEvent (ClipboardEvent) as Exports
import Web.Event.Internal.Types (Event) as Exports
import Web.HTML.Event.DragEvent (DragEvent) as Exports
import Web.PointerEvent (PointerEvent) as Exports
import Web.TouchEvent.TouchEvent (TouchEvent) as Exports
import Web.UIEvent.FocusEvent (FocusEvent) as Exports
import Web.UIEvent.KeyboardEvent (KeyboardEvent) as Exports
import Web.UIEvent.MouseEvent (MouseEvent) as Exports
import Web.UIEvent.WheelEvent (WheelEvent) as Exports

-- | An `IProp` is an HTML property, with kind
-- | `IProp :: Row Type -> Type -> Type`, where the row type is the set of all
-- | valid properties for the element that this property will be attached to.
-- |
-- | Typically, an event listener type would look like this:
-- | ```
-- | onMouseMove
-- |   :: forall r i
-- |    . (MouseEvent -> i)
-- |   -> IProp (onMouseMove :: MouseEvent | r) i
-- | ```
-- | where the event type is predetermined and the row type is parameterized to
-- | allow any element with an `onMouseMove` property.
-- |
-- | This generic `EventProp` is the inverse: parameterized to allow any event
-- | type, but the property must be a valid property of the `HTMLcanvas` row.
type EventProp event i = (event -> i) -> IProp HTMLcanvas i

-- | An event handler is a variant of an update function, which receives an
-- | event and produces an update function in response.
type Handler event localState = event -> App.UpdateFunction localState

-- | An Interaction is a combination event listener and handler which is turned
-- | into an HTML property and attached to a Gesso canvas. They can be
-- | constructed with the "`on`" functions (`onMouseMove`, `onKeyDown`, etc.)
data Interaction event localState =
  Interaction (forall i. EventProp event i) (Handler event localState)

-- | `Interactions` is a record containing arrays of interactions for each type
-- | of event that Canvas supports. It's used in
-- | [`Gesso.Application.AppBehavior`](Gesso.Application.html#t:AppBehavior)
-- | to add event handlers to a component.
type Interactions localState =
  { base :: Array (Interaction Exports.Event localState)
  , clipboard :: Array (Interaction Exports.ClipboardEvent localState)
  , focus :: Array (Interaction Exports.FocusEvent localState)
  , keyboard :: Array (Interaction Exports.KeyboardEvent localState)
  , touch :: Array (Interaction Exports.TouchEvent localState)
  , drag :: Array (Interaction Exports.DragEvent localState)
  , mouse :: Array (Interaction Exports.MouseEvent localState)
  , wheel :: Array (Interaction Exports.WheelEvent localState)
  , pointer :: Array (Interaction Exports.PointerEvent localState)
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
