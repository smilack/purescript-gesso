-- | This module contains constructors for Interactions for event properties
-- | that Canvas supports. The list of properties comes from
-- | [`DOM.HTML.Indexed.HTMLcanvas`](https://pursuit.purescript.org/packages/purescript-dom-indexed/7.0.0/docs/DOM.HTML.Indexed#t:HTMLcanvas).
-- | A number of those event properties are not currently exported by
-- | `Halogen.HTML.Events`. For completeness, they are included here but
-- | commented out.
module Gesso.Interactions.Events
  ( onAuxClick
  , onBeforeInput
  , onBlur
  , onClick
  , onContextMenu
  , onCopy
  , onCut
  , onDoubleClick
  , onDrag
  , onDragEnd
  , onDragEnter
  , onDragExit
  , onDragLeave
  , onDragOver
  , onDragStart
  , onDrop
  , onFocus
  , onFocusIn
  , onFocusOut
  , onGotPointerCapture
  , onInput
  , onKeyDown
  , onKeyUp
  , onLostPointerCapture
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOut
  , onMouseOver
  , onMouseUp
  , onPaste
  , onPointerCancel
  , onPointerDown
  , onPointerEnter
  , onPointerLeave
  , onPointerMove
  , onPointerOut
  , onPointerOver
  , onPointerUp
  , onTouchCancel
  , onTouchEnd
  , onTouchEnter
  , onTouchLeave
  , onTouchMove
  , onTouchStart
  , onTransitionEnd
  , onWheel
  ) where

import Prelude

import Gesso.Interactions.Internal (ClipboardEvent, DragEvent, Event, FocusEvent, Handler, Interaction(..), KeyboardEvent, MouseEvent, PointerEvent, TouchEvent, WheelEvent)
import Halogen.HTML.Events (handler, onAuxClick, onBlur, onClick, onCopy, onCut, onDoubleClick, onDrag, onDragEnd, onDragEnter, onDragExit, onDragLeave, onDragOver, onDragStart, onDrop, onFocus, onFocusIn, onFocusOut, onInput, onKeyDown, onKeyUp, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver, onMouseUp, onPaste, onTouchCancel, onTouchEnd, onTouchEnter, onTouchLeave, onTouchMove, onTouchStart, onTransitionEnd, onWheel) as HE
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))

onAuxClick :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onAuxClick = Interaction HE.onAuxClick

onBeforeInput :: forall s. Handler Event s -> Interaction Event s
onBeforeInput = Interaction (HE.handler (EventType "beforeinput"))

onBlur :: forall s. Handler FocusEvent s -> Interaction FocusEvent s
onBlur = Interaction HE.onBlur

onClick :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onClick = Interaction HE.onClick

onContextMenu :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onContextMenu = Interaction (HE.handler (EventType "contextmenu") <<< pointerHandler)

onCopy :: forall s. Handler ClipboardEvent s -> Interaction ClipboardEvent s
onCopy = Interaction HE.onCopy

onCut :: forall s. Handler ClipboardEvent s -> Interaction ClipboardEvent s
onCut = Interaction HE.onCut

onDoubleClick :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onDoubleClick = Interaction HE.onDoubleClick

onDrag :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDrag = Interaction HE.onDrag

onDragEnd :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragEnd = Interaction HE.onDragEnd

onDragEnter :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragEnter = Interaction HE.onDragEnter

onDragExit :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragExit = Interaction HE.onDragExit

onDragLeave :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragLeave = Interaction HE.onDragLeave

onDragOver :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragOver = Interaction HE.onDragOver

onDragStart :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDragStart = Interaction HE.onDragStart

onDrop :: forall s. Handler DragEvent s -> Interaction DragEvent s
onDrop = Interaction HE.onDrop

onFocus :: forall s. Handler FocusEvent s -> Interaction FocusEvent s
onFocus = Interaction HE.onFocus

onFocusIn :: forall s. Handler FocusEvent s -> Interaction FocusEvent s
onFocusIn = Interaction HE.onFocusIn

onFocusOut :: forall s. Handler FocusEvent s -> Interaction FocusEvent s
onFocusOut = Interaction HE.onFocusOut

onGotPointerCapture :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onGotPointerCapture = Interaction (HE.handler (EventType "gotpointercapture") <<< pointerHandler)

onInput :: forall s. Handler Event s -> Interaction Event s
onInput = Interaction HE.onInput

onKeyDown :: forall s. Handler KeyboardEvent s -> Interaction KeyboardEvent s
onKeyDown = Interaction HE.onKeyDown

onKeyUp :: forall s. Handler KeyboardEvent s -> Interaction KeyboardEvent s
onKeyUp = Interaction HE.onKeyUp

onLostPointerCapture :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onLostPointerCapture = Interaction (HE.handler (EventType "lostpointercapture") <<< pointerHandler)

onMouseDown :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseDown = Interaction HE.onMouseDown

onMouseEnter :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseEnter = Interaction HE.onMouseEnter

onMouseLeave :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseLeave = Interaction HE.onMouseLeave

onMouseMove :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseMove = Interaction HE.onMouseMove

onMouseOut :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseOut = Interaction HE.onMouseOut

onMouseOver :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseOver = Interaction HE.onMouseOver

onMouseUp :: forall s. Handler MouseEvent s -> Interaction MouseEvent s
onMouseUp = Interaction HE.onMouseUp

onPaste :: forall s. Handler ClipboardEvent s -> Interaction ClipboardEvent s
onPaste = Interaction HE.onPaste

onPointerCancel :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerCancel = Interaction (HE.handler (EventType "pointercancel") <<< pointerHandler)

onPointerDown :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerDown = Interaction (HE.handler (EventType "pointerdown") <<< pointerHandler)

onPointerEnter :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerEnter = Interaction (HE.handler (EventType "pointerenter") <<< pointerHandler)

onPointerLeave :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerLeave = Interaction (HE.handler (EventType "pointerleave") <<< pointerHandler)

onPointerMove :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerMove = Interaction (HE.handler (EventType "pointermove") <<< pointerHandler)

onPointerOut :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerOut = Interaction (HE.handler (EventType "pointerout") <<< pointerHandler)

onPointerOver :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerOver = Interaction (HE.handler (EventType "pointerover") <<< pointerHandler)

onPointerUp :: forall s. Handler PointerEvent s -> Interaction PointerEvent s
onPointerUp = Interaction (HE.handler (EventType "pointerup") <<< pointerHandler)

onTouchCancel :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchCancel = Interaction HE.onTouchCancel

onTouchEnd :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchEnd = Interaction HE.onTouchEnd

onTouchEnter :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchEnter = Interaction HE.onTouchEnter

onTouchLeave :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchLeave = Interaction HE.onTouchLeave

onTouchMove :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchMove = Interaction HE.onTouchMove

onTouchStart :: forall s. Handler TouchEvent s -> Interaction TouchEvent s
onTouchStart = Interaction HE.onTouchStart

onTransitionEnd :: forall s. Handler Event s -> Interaction Event s
onTransitionEnd = Interaction HE.onTransitionEnd

onWheel :: forall s. Handler WheelEvent s -> Interaction WheelEvent s
onWheel = Interaction HE.onWheel

-- Based on the handler functions in Halogen.HTML.Events:
-- https://github.com/purescript-halogen/purescript-halogen/blob/222e1febc889c64a71013748b6bf04db969888f9/src/Halogen/HTML/Events.purs#L266-L285
pointerHandler :: forall i. (PointerEvent -> i) -> Event -> i
pointerHandler = unsafeCoerce
