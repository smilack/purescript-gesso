module Gesso.Interactions
  ( EventProp
  , Handler
  , Interaction
  , Interactions
  , default
  , mkInteraction
  , toProps
  , mousePosition
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed (HTMLcanvas)
import Gesso.Dimensions as Dims
import Gesso.Interactions.Events as Events
import Gesso.Interactions.Events (Event, ClipboardEvent, FocusEvent, KeyboardEvent, TouchEvent, DragEvent, MouseEvent, WheelEvent)
import Halogen.HTML.Properties (IProp)

type EventProp event i
  = (event -> Maybe i) -> IProp HTMLcanvas i

type Handler event appState
  = event -> appState -> appState

-- I need to be able to make a list of Interaction
-- but 'event' could be different for each entry
-- so I think I can't make event a type variable
-- i.e. no 'Interaction event appState i'
-- I think I need to import all the event types
-- and make a variant for each type...
--   data Interaction event appState i
--     = Interaction (Event event i) (Handler event appState)
data Interaction event appState i
  = Interaction (EventProp event i) (Handler event appState)

mkInteraction ::
  forall event appState i.
  EventProp event i -> Handler event appState -> Interaction event appState i
mkInteraction = Interaction

-- basicEvent :: forall appState i. EventProp Event i -> Handler Event appState -> Interaction appState i
-- basicEvent = Base
-- clipboardEvent :: forall appState i. EventProp ClipboardEvent i -> Handler ClipboardEvent appState -> Interaction appState i
-- clipboardEvent = Clipboard
-- focusEvent :: forall appState i. EventProp FocusEvent i -> Handler FocusEvent appState -> Interaction appState i
-- focusEvent = Focus
-- keyboardEvent :: forall appState i. EventProp KeyboardEvent i -> Handler KeyboardEvent appState -> Interaction appState i
-- keyboardEvent = Keyboard
-- touchEvent :: forall appState i. EventProp TouchEvent i -> Handler TouchEvent appState -> Interaction appState i
-- touchEvent = Touch
-- dragEvent :: forall appState i. EventProp DragEvent i -> Handler DragEvent appState -> Interaction appState i
-- dragEvent = Drag
-- mouseEvent :: forall appState i. EventProp MouseEvent i -> Handler MouseEvent appState -> Interaction appState i
-- mouseEvent = Mouse
-- wheelEvent :: forall appState i. EventProp WheelEvent i -> Handler WheelEvent appState -> Interaction appState i
-- wheelEvent = Wheel
mousePosition ::
  forall moreState i.
  Interaction MouseEvent { mousePos :: Maybe { x :: Number, y :: Number } | moreState } i
mousePosition = mkInteraction Events.onMouseMove getMousePos
  where
  getMousePos event state =
    let
      point = Dims.fromMouseEvent event
    in
      state
        { mousePos = Just { x: Dims.getX point, y: Dims.getY point } }

-- run ::
--   forall event appState i.
--   (forall e. Handler e appState -> e -> Maybe i) ->
--   Interaction event appState i ->
--   IProp HTMLcanvas i
-- run process (Interaction onEvent handler) =
--   onEvent
--     $ handler
toProps ::
  forall appState i.
  ((appState -> appState) -> Maybe i) ->
  Interactions appState i -> Array (IProp HTMLcanvas i)
toProps toCallback interactions =
  -- I tried to put these all in an array and foldMap it,
  --   but it didn't work since they're different types
  map toProp base
    <> map toProp clipboard
    <> map toProp focus
    <> map toProp keyboard
    <> map toProp touch
    <> map toProp drag
    <> map toProp mouse
    <> map toProp wheel
  where
  { base, clipboard, focus, keyboard, touch, drag, mouse, wheel } = interactions

  toProp :: forall e. Interaction e appState i -> IProp HTMLcanvas i
  toProp (Interaction onEvent handler) = onEvent $ toCallback <<< handler

--how do I get the right event types?
--do I need an array for each event type?
-- type Interactions appState i
--   = { mouseInteractions :: Interaction MouseEvent appState i
--     , keyboardInteractions :: Interaction KeyboardEvent appState i
--     , ...
--     }
-- Originally I wanted to have `Interaction event appState i` and a
--   list of interactions, but because two interactions could have
--   different event types, the list wouldn't typecheck.
-- Then I tried `Interaction appState i` with a variant for each event,
--   but I need to write a function to combine with a
--   `Handler event appState -> event -> Maybe i` from Canvas, and I
--   couldn't get the event property and handler out of the Interaction
--   while keeping it general enough to match the Canvas function.
-- I think, barring some trick I'm not aware of, I have to separate the
--   Interactions into separate lists for each event.
type Interactions appState i
  = { base :: Array (Interaction Event appState i)
    , clipboard :: Array (Interaction ClipboardEvent appState i)
    , focus :: Array (Interaction FocusEvent appState i)
    , keyboard :: Array (Interaction KeyboardEvent appState i)
    , touch :: Array (Interaction TouchEvent appState i)
    , drag :: Array (Interaction DragEvent appState i)
    , mouse :: Array (Interaction MouseEvent appState i)
    , wheel :: Array (Interaction WheelEvent appState i)
    }

default :: forall appState i. Interactions appState i
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
