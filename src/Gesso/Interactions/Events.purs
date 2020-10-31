-- | This module re-exports a number of event types and properties that Canvas
-- | supports. The list of properties comes from
-- | [`purescript-dom-indexed`](https://pursuit.purescript.org/packages/purescript-dom-indexed/7.0.0/docs/DOM.HTML.Indexed#t:HTMLcanvas).
-- | There are several events that are not exported because they are not found
-- | in Halogen: `onContextMenu`, `onKeyPress`, `gotPointerCapture`,
-- | `lostPointerCapture`, `onPointerCancel`, `onPointerDown`, `onPointerEnter`,
-- | `onPointerLeave`, `onPointerMove`, `onPointerOut`, `onPointerOver`,
-- | and `onPointerUp`.
module Gesso.Interactions.Events
  ( module Web.Event.Internal.Types
  , module Web.Clipboard.ClipboardEvent
  , module Web.UIEvent.FocusEvent
  , module Web.UIEvent.KeyboardEvent
  , module Web.TouchEvent.TouchEvent
  , module Web.HTML.Event.DragEvent
  , module Web.UIEvent.MouseEvent
  , module Web.UIEvent.WheelEvent
  , module Halogen.HTML.Events
  ) where

import Web.Event.Internal.Types (Event)
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Halogen.HTML.Events
  ( onCopy
  , onCut
  , onPaste
  , onBlur
  , onFocus
  , onFocusIn
  , onFocusOut
  , onTransitionEnd
  , onKeyDown
  , onKeyUp
  , onTouchCancel
  , onTouchEnd
  , onTouchEnter
  , onTouchLeave
  , onTouchMove
  , onTouchStart
  , onDrag
  , onDragEnd
  , onDragEnter
  , onDragExit
  , onDragLeave
  , onDragOver
  , onDragStart
  , onDrop
  , onClick
  , onDoubleClick
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOut
  , onMouseOver
  , onMouseUp
  , onWheel
  )
