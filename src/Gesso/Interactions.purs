module Gesso.Interactions
  ( EventProp
  , Handler
  , FullHandler
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

type FullHandler localState
  = Dims.Scaler -> localState -> Maybe localState

type Handler event localState
  = event -> FullHandler localState

data Interaction event localState i
  = Interaction (EventProp event i) (Handler event localState)

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
type Interactions localState i
  = { base :: Array (Interaction Event localState i)
    , clipboard :: Array (Interaction ClipboardEvent localState i)
    , focus :: Array (Interaction FocusEvent localState i)
    , keyboard :: Array (Interaction KeyboardEvent localState i)
    , touch :: Array (Interaction TouchEvent localState i)
    , drag :: Array (Interaction DragEvent localState i)
    , mouse :: Array (Interaction MouseEvent localState i)
    , wheel :: Array (Interaction WheelEvent localState i)
    }

toProps ::
  forall localState i.
  (FullHandler localState -> Maybe i) ->
  Interactions localState i -> Array (IProp HTMLcanvas i)
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
  toProp :: forall e. Interaction e localState i -> IProp HTMLcanvas i
  toProp (Interaction onEvent handler) = onEvent $ toCallback <<< handler

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

mkInteraction ::
  forall event localState i.
  EventProp event i -> Handler event localState -> Interaction event localState i
mkInteraction = Interaction

mousePosition ::
  forall moreState i.
  Interaction MouseEvent { mousePos :: Maybe { x :: Number, y :: Number } | moreState } i
mousePosition = mkInteraction Events.onMouseMove getMousePos
  where
  getMousePos event _ state =
    let
      point = Dims.fromMouseEvent event
    in
      Just state { mousePos = Just { x: Dims.getX point, y: Dims.getY point } }
