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

data Interaction event appState i
  = Interaction (EventProp event i) (Handler event appState)

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

toProps ::
  forall appState i.
  ((appState -> appState) -> Maybe i) ->
  Interactions appState i -> Array (IProp HTMLcanvas i)
toProps toCallback { base, clipboard, focus, keyboard, touch, drag, mouse, wheel } =
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
  toProp :: forall e. Interaction e appState i -> IProp HTMLcanvas i
  toProp (Interaction onEvent handler) = onEvent $ toCallback <<< handler

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

mkInteraction ::
  forall event appState i.
  EventProp event i -> Handler event appState -> Interaction event appState i
mkInteraction = Interaction

mousePosition ::
  forall moreState i.
  Interaction MouseEvent { mousePos :: Maybe { x :: Number, y :: Number } | moreState } i
mousePosition = mkInteraction Events.onMouseMove getMousePos
  where
  getMousePos event state =
    let
      point = Dims.fromMouseEvent event
    in
      state { mousePos = Just { x: Dims.getX point, y: Dims.getY point } }
