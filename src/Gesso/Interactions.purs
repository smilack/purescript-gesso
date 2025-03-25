-- | Interactions are a wrapper for HTML events that allow specifying event
-- | types and handlers which are attached to the canvas when it's created.
-- | Interactions are part of the `AppSpec` and will typically be specified as
-- | an update of the [`default`](#v:default) record, similar to `mkEval` and
-- | `defaultEval` in Halogen.
-- |
-- | The [`Interactions`](#t:Interactions) type is a record of arrays of
-- | interactions for each event type.
module Gesso.Interactions
  ( ClipboardInteraction
  , DragInteraction
  , EventInteraction
  , FocusInteraction
  , KeyboardInteraction
  , MouseInteraction
  , PointerInteraction
  , TouchInteraction
  , WheelInteraction
  , default
  , module Exports
  ) where

import Gesso.Interactions.Internal (Interactions, Interaction)
import Gesso.Interactions.Internal (ClipboardEvent, DragEvent, Event, FocusEvent, Handler, Interaction, Interactions, KeyboardEvent, MouseEvent, PointerEvent, TouchEvent, WheelEvent) as Exports
import Gesso.Interactions.Events (onAuxClick, onBlur, onClick, onCopy, onCut, onDoubleClick, onDrag, onDragEnd, onDragEnter, onDragExit, onDragLeave, onDragOver, onDragStart, onDrop, onFocus, onFocusIn, onFocusOut, onInput, onKeyDown, onKeyUp, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver, onMouseUp, onPaste, onTouchCancel, onTouchEnd, onTouchEnter, onTouchLeave, onTouchMove, onTouchStart, onTransitionEnd, onWheel) as Exports

-- | A default `Interactions` record containing no interactions. The attributes
-- | can be overridden individually instead of manually creating a complete but
-- | mostly empty record. For example,
-- | ```
-- | Interactions.default { keyboard = [ handleKeyDown ] }
-- | ```
default :: forall state. Interactions state
default =
  { base: []
  , clipboard: []
  , focus: []
  , keyboard: []
  , touch: []
  , drag: []
  , mouse: []
  , wheel: []
  , pointer: []
  }

type ClipboardInteraction state = Interaction Exports.ClipboardEvent state

type DragInteraction state = Interaction Exports.DragEvent state

type EventInteraction state = Interaction Exports.Event state

type FocusInteraction state = Interaction Exports.FocusEvent state

type KeyboardInteraction state = Interaction Exports.KeyboardEvent state

type MouseInteraction state = Interaction Exports.MouseEvent state

type PointerInteraction state = Interaction Exports.PointerEvent state

type TouchInteraction state = Interaction Exports.TouchEvent state

type WheelInteraction state = Interaction Exports.WheelEvent state
