-- | Interactions are a wrapper for HTML events that allow specifying event
-- | types and handlers which are attached to the drawing screen when it
-- | renders. Interactions are an input to the Canvas component, and will
-- | typically be specified as an update of the [`default`](#v:default) record,
-- | similar to `mkEval` and `defaultEval` in Halogen.
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
  , pointer: []
  }

type ClipboardInteraction s = Interaction Exports.ClipboardEvent s

type DragInteraction s = Interaction Exports.DragEvent s

type EventInteraction s = Interaction Exports.Event s

type FocusInteraction s = Interaction Exports.FocusEvent s

type KeyboardInteraction s = Interaction Exports.KeyboardEvent s

type MouseInteraction s = Interaction Exports.MouseEvent s

type PointerInteraction s = Interaction Exports.PointerEvent s

type TouchInteraction s = Interaction Exports.TouchEvent s

type WheelInteraction s = Interaction Exports.WheelEvent s
