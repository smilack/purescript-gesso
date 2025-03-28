# The Gesso Manual

### Contents

1. [Launching a Gesso Application](#launching-a-gesso-application)
   1. [`launch` and `launchIn`](#launch-and-launchin)
   2. [`runGessoAff`](#rungessoaff)
2. [`AppSpec` Record](#appspec-record)
   1. [Basic `AppSpec` Fields](#basic-appspec-fields)
   2. [Canvas Dimensions](#canvas-dimensions)
3. [`AppBehavior` Record](#appbehavior-record)
   1. [Rendering Functions](#rendering-functions)
   2. [Update Functions and Events](#update-functions-and-events)
   3. [Per-Frame vs Fixed-Rate Updates](#per-frame-vs-fixed-rate-updates)
   4. [Interactions](#interactions)
   5. [Component Input and Output](#component-input-and-output)
   6. [Update Timing](#update-timing)
4. [`Geometry` Module](#geometry-module)
   1. [Size and Positioning Types](#size-and-positioning-types)
   2. [`Scaler` and `Scalers` Records](#scaler-and-scalers-records)
   3. [Calling Scaling Functions](#calling-scaling-functions)
      1. [Scaling a Single Value](#scaling-a-single-value)
      2. [Scaling a Record](#scaling-a-record)
      3. [Flipped Scaling Functions](#flipped-scaling-functions)
      4. [Scaling Function Operators](#scaling-function-operators)
   4. [Other Geometry Functions](#other-geometry-functions)
5. [Gesso as a Halogen Component](#gesso-as-a-halogen-component)
   1. [Halogen Component Input (Queries)](#halogen-component-input-queries)
   2. [Halogen Component Output](#halogen-component-output)

# Launching a Gesso Application

The `Gesso` module contains functions for launching a standalone Gesso application. Launching a Gesso application always requires an [`AppSpec` record](#appspec-record).

> [!TIP]
> If the Gesso component is going to be part of a larger Halogen application, this module isn't necessary. See [Gesso as a Halogen Component](#gesso-as-a-halogen-component).

## `launch` and `launchIn`

These are the simplest options, designed for applications which require no `Aff` effects other than Gesso.

`launch` is perfect for applications with nothing else on the page. Gesso attaches directly to the page body:

```purescript
launch :: forall state i o. AppSpec state i o -> Effect Unit

main :: Effect Unit
main = launch appSpec
```

`launchIn` is best for pages with some static content. It takes a `String` as an argument, which is treated as a query selector to find an element on the page to attach to:

```purescript
launchIn :: forall state i o. String -> AppSpec state i o -> Effect Unit

main :: Effect Unit
main = launchIn "#some-element-id" appSpec
```

## `runGessoAff`

`runGessoAff` is an alias for `runHalogenAff`. It's the most flexible way to launch Gesso because it allows running other `Aff` effects while setting up Gesso. For example, this is roughly what `launch` does:

```purescript
import Gesso.Canvas (component)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  _ <- runUI component appSpec body
  pure unit
```

# `AppSpec` Record

In `Gesso.Application`, the `AppSpec` and `AppBehavior` types contain everything that makes an application work.

```purescript
type AppSpec state input output =
  { name :: String
  , initialState :: state
  , viewBox :: Rect
  , window :: WindowMode
  , behavior :: AppBehavior state input output
  }
```

## Basic `AppSpec` Fields

The `state` type is the state of your application and can be anything you want, for example, a large complicated record, a single integer, or just `unit` if you don't need to track state at all

The `input` and `output` types are only used for communication with a parent component in a Halogen application. See [Gesso as a Halogen Component](#gesso-as-a-halogen-component).

The `name` field will be used as the `id` attribute for the canvas element.

> [!WARNING]
> If you plan to target the element with any CSS or JavaScript outside of what Gesso normally does, then it's best to make `name` a valid CSS identifier. Otherwise, it doesn't matter much.

## Canvas Dimensions

`viewBox` is a `Rect` ([Size and Positioning Types](#size-and-positioning-types)) that determines the coordinate system of the drawing. It is analagous to the `viewBox` attribute on an SVG — neither is tied to the actual size of the element on the page. This simplifies the drawing process when the screen size is unpredictable and subject to change.

`window` determines the space that the canvas element takes up:
- `Fixed` creates an element with an exact size.
- `Stretch` causes the element to fill its parent.
- `FullScreen` takes up the entire page from the top left corner to the bottom right.

The `viewBox` scales automatically to fit within the canvas element while remaining centered. Unless the view box and drawing have the exact same aspect ratio, this leaves a margin in the canvas outside of the view box on one axis. (That is, it behaves like SVG's `preserveAspectRatio="xMidYMid meet"`)

# `AppBehavior` Record

The `AppBehavior` type covers all functions that make an application interact with or respond to the canvas itself, events, other components, and the passage of time.

```purescript
type AppBehavior state input output =
  { render :: RenderFunction state
  , update :: UpdateFunction state
  , fixed :: FixedUpdate state
  , interactions :: Interactions state
  , output :: OutputProducer state output
  , input :: InputReceiver state input
  }
```

`Gesso.Application` exports a default `AppBehavior` record, which can be updated piecemeal, for example:

```purescript
myAppBehavior :: AppBehavior MyState MyInput MyOutput
myAppBehavior = defaultBehavior { render = render, update = update }
```

All of these functions run in `Effect` and therefore have access to any `Effect`.

There are several important types in the arguments to these functions. They'll be covered in more detail later, but for a quick rundown:

- `Delta`: the timestamps of the current and previous animation frames and the difference between them
- `Scalers`: information about the sizes of the canvas and the drawing (view box) and functions to convert between the two
- `States`: two sequential states (the current and previous) and the progress (on the interval `[0, 1]`) from the first to second
- `Compare`: two states — an old and new — not necessarily sequential

## Rendering Functions

```purescript
type RenderFunction state = Context2D -> Delta -> Scalers -> States state -> Effect Unit
```

`render` is the only behavior function that has access to the canvas's `Context2D` for drawing.

The `States` record contains the current and previous states and an interpolation parameter. Typically, only the current is needed, and the interpolation parameter is `1`.

However, if there is a fixed-interval update function running at a different rate than the rendering function, the interpolation parameter will be a number in the range `[0, 1]` representing the progress from the previous state to the current. This can be used to smooth animations in some circumstances.

## Update Functions and Events

Because of the overlap between kinds of state-changing functions, they use a few type synonyms to maintain consistency. This is not how they are literally written in the code, but it may be the most clear presentation:

```purescript
type UpdateFunction state = Delta -> Scalers -> local -> Effect (Maybe local)

-- Gesso.Interactions
type Handler event state = event -> UpdateFunction state
```

All kinds of state-changing functions have access to the same `Delta` and `Scalers` records as `render`, but only one state.

All of them also return a `Maybe state`, with a `Nothing` value indicating that no change was made.

### Per-Frame vs Fixed-Rate Updates

`update` is the most basic kind of update function. It runs once per frame immediately before `render`.

`fixed` updates have a time interval in milliseconds (constructed using the `Gesso.Time.hz` function) and an update function. Gesso tracks the last time that the fixed update function ran (`last`). Each frame, if the amount of time since `last` is greater than `interval`, the fixed update is run repeatedly, with timestamps starting at `last + interval` and increasing by `interval`, stopping before `last + i * interval` would pass the current time. (See [GameProgrammingPatterns.com: Sequencing Patterns / Play catch up](https://gameprogrammingpatterns.com/game-loop.html#play-catch-up))

> [!WARNING]
> There isn't an escape hatch to extend or skip fixed updates if the update function takes longer than `interval` to run. (Issue #24) Very small intervals or very slow fixed update functions could cause the application to get stuck trying to catch up.

## Interactions

Interactions are event handlers attached to the canvas. The `interactions` field is a record containing an array for each event type:

```purescript
type Interactions state =
  { base :: Array (EventInteraction state)
  , clipboard :: Array (ClipboardInteraction state)
  , focus :: Array (FocusInteraction state)
  , keyboard :: Array (KeyboardInteraction state)
  , touch :: Array (TouchInteraction state)
  , drag :: Array (DragInteraction state)
  , mouse :: Array (MouseInteraction state)
  , wheel :: Array (WheelInteraction state)
  , pointer :: Array (PointerInteraction state)
  }
```

In addition to the usual state-changing function signature, interactions have access to the triggering event. The event types come from several different modules. `Gesso.Interactions` re-exports all of these, but their original modules contain many functions for working with them:

```purescript
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Event.Internal.Types (Event)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.PointerEvent (PointerEvent)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
```

Interaction constructors are re-exported in the `Gesso.Interactions` module but you can look at `Gesso.Interactions.Events` for a complete list. A small number of canvas events are not implemented yet, but they are included with comments for completeness.

Interaction constructors take a `Handler event state` function and return a type of interaction specific to that event (e.g. `onMouseDown :: forall s. Handler MouseEvent s -> MouseInteraction s`). A default record containing no interactions is provided for convenience.

Here is an example of creating an event handler and adding it to an `Interactions` record:

```purescript
import Web.UIEvent.MouseEvent (MouseEvent)
import Gesso.Geometry (Point, fromMouseEvent)
import Gesso.Interactions (Interactions, MouseInteraction, default, onMouseMove)

type State = Point

appInteractions :: Interactions State
appInteractions = default { mouse = [ trackMousePosition ] }

trackMousePosition :: MouseInteraction State
trackMousePosition = onMouseMove getMousePosition

getMousePosition :: MouseEvent -> Delta -> Scalers -> state -> Effect (Maybe state)
getMousePosition event _ _ _ = pure $ Just $ fromMouseEvent event
```

The default `Interactions` record is already included in the default `AppBehavior` record, so it can be updated at the same time the rest of the `AppSpec` is defined.

## Component Input and Output

The `input` and `output` functions control component I/O between a Gesso component and a parent component in a Halogen application. See [Gesso as a Halogen Component](#gesso-as-a-halogen-component).

## Update Timing

Interactions and component inputs are timestamped as they arrive, and their `Delta` values are based on the difference between this time and the time of the last frame rendering.

On each animation frame, after Gesso determines the timing of any necessary fixed update function calls, interactions, component inputs, and fixed updates are sorted by timestamp before processing.

# `Geometry` Module

## Size and Positioning Types

`Gesso.Geometry` contains three pairs of Row and Record types that may be useful. The Row types are open and have a type parameter:

```purescript
type Position a r = ( x :: a, y :: a | r )

type Size a r = ( width :: a, height :: a | r )

type Rectangular a r = Position a + Size a + r
```

Each Row has a corresponding closed record with the type specified as `Number`:

```purescript
type Point = { | Position Number () }

type Area = { | Size Number () }

type Rect = { | Rectangular Number () }
```

These are used in a handful of places internally, and some external modules, like `Graphics.Canvas`, use records with the same fields for functions like `fillRect` and `clearRect`.

There is also a default, empty value for each record:

```purescript
origin :: Point
origin = { x: 0.0, y: 0.0 }

sizeless :: Area
sizeless = { width: 0.0, height: 0.0 }

null :: Rect
null = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
```

## `Scaler` and `Scalers` Records

Because the size of a user's screen or browser window is unpredictable, it's useful to set a view box in the `AppSpec` so that drawing coordinates can be consistent. However, this means that it's necessary to convert from drawing coordinates to canvas coordinates in order to paint the canvas, and to convert from canvas to drawing to process mouse or touch events. In addition, because the view box scales by preserving its aspect ratio while remaining centered, there may be a margin to account for — horizontally or vertically.

> [!IMPORTANT]
> The `viewBox` record determines two things:
> 1. the scale and position of the drawing coordinates relative to the canvas coordinates
> 2. the area of the drawing that must always be visible
>
> The drawing coordinate system extends infinitely, which means that canvas coordinates outside the view box can still be converted to valid drawing coordinates. Drawings in the margins may be visible, but drawings are only *guaranteed* to be visible if they fall within the view box.

The `Scalers` record contains data and functions to simplify all of these conversions.

```purescript
type Scalers =
  { scale :: Number
  , canvas :: Scaler
  , drawing :: Scaler
  }
```

`scale` is a constant scaling factor: the amount that the view box has been scaled up or down to fit within the canvas.

`canvas` and `drawing` are:

```purescript
-- with some synonyms expanded:
type Scaler =
  { rect :: Rect
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , scaling ::
      { all :: forall rl r. RowToList r rl => Scalable rl r Number => {| r } -> Builder {} {| r }
      , x :: Number -> Number
      , y :: Number -> Number
      , length :: Number -> Number
      }
  }
```

The `x`/`y`/`width`/`height` fields are the same as the `rect` field, but repeated to make it easier to get a single attribute or `Rect` as needed.

In `drawing`, these fields are identical to the view box. In `canvas`, `x` and `y` are zero and `width` and `height` are the dimensions of the canvas.

> [!TIP]
> The `Scalers` record is automatically rebuilt whenever the browser window is resized.

## Calling Scaling Functions

The `scaling` field of a `Scaler` contains functions for scaling to the coordinate system with the same name as the record. It's not recommended to call the scaling functions directly. Instead, use these functions from `Geometry`:

```purescript
xTo :: Number -> Scaler -> Number

yTo :: Number -> Scaler -> Number

lengthTo :: Number -> Scaler -> Number

to :: forall rl r. RowToList r rl => Scalable rl r Number => {| r } -> Scaler -> {| r }
```

### Scaling a Single Value

`xTo`, `yTo`, and `lengthTo` operate on single values. For example, if you have a circle with radius `1.0` in your view box at coordinates `(2.0, 3.0)`, you could convert those values to canvas coordinates like this:

```purescript
x' = 2.0 `xTo` canvas
y' = 3.0 `yTo` canvas
r' = 1.0 `lengthTo` canvas
```

(What makes `lengthTo` different from the others is that lengths don't need to account for page margins.)

### Scaling a Record

The `to` function is provided to greatly simplify scaling multiple values:

```purescript
circle' = { x: 2.0, y: 3.0, r: 1.0 } `to` canvas
```

Its type signature is so abstract because it can operate on any record and automatically convert many different fields. Currently, it will convert any of these fields if they have type `Number`:

| Conversion | Field name |
|-|-|
| `xTo` | `x`, `x1`, `x2` |
| `yTo` | `y`, `y1`, `y2` |
| `lengthTo` | `width`, `w`, `height`, `h`, `radius`, `r`, `length`, `len`, `l` |

### Flipped Scaling Functions

`to`, `xTo`, `yTo`, and `lengthTo` have `from` counterparts with flipped arguments, e.g.:

```purescript
xTo :: Number -> Scaler -> Number

xFrom :: Scaler -> Number -> Number
xFrom = flip xTo
```

This can be more convenient sometimes, depending on code formatting, or when composing functions.

### Scaling Function Operators

The scaling functions have infix operators as well:

| | `to` | `from` |
|-|-|-|
| all | `*~>` | `<~*` |
| `x` | `-~>` | `<~-` |
| `y` | `\|~>` | `<~\|` |
| `length` | `/~>` | `<~/` |

For example:

```purescript
x' = 2.0 -~> canvas
circle' = canvas <~* { x: 2.0, y: 3.0, r: 1.0 }
```

## Other Geometry Functions

Geometry exports a `fromMouseEvent` function that extracts a `Point` (in canvas coordinates) from a `MouseEvent`.

# Gesso as a Halogen Component

See the [Halogen Guide: Parent and Child Components](https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html) for adding a child to a Halogen component.

> [!IMPORTANT]
> Halogen and Gesso use slightly different terminology here.
>
> In Halogen, child components can have a `receive :: input -> Maybe action` function that's called on every render, and "Input" refers to this function. "Queries" are messages sent from a parent to a child outside of this cycle, and "Output" is messages sent from a child to a parent.
>
> The Gesso component doesn't use a `receive` function, so "Queries" are referred to as "Input" for symmetry with "Output."

`Gesso.Canvas` provides a `Slot` type which includes the `CanvasInput` and `CanvasOutput` types used for I/O, as well as a proxy for the row label:

```purescript
type Slot input output slot = H.Slot (CanvasInput input) (CanvasOutput output) slot

_gessoCanvas = Proxy :: Proxy "gessoCanvas"
```

You'll need to define an input and an output type (which can be the same) that will be reflected in the `AppSpec`. Recall the `AppSpec` and `AppBehavior` types:

```purescript
type AppSpec state input output =
  { name :: String
  , initialState :: state
  , viewBox :: Rect
  , window :: WindowMode
  , behavior :: AppBehavior state input output
  }

type AppBehavior state input output =
  { render :: RenderFunction state
  , update :: UpdateFunction state
  , fixed :: FixedUpdate state
  , interactions :: Interactions state
  , output :: OutputProducer state output
  , input :: InputReceiver state input
  }
```

## Halogen Component Input (Queries)

`InputReceiver` is an update function that also receives a copy of the input type. Apart from that, it behaves the same as an update or event handler.

```purescript
type InputReceiver state input = input -> Delta -> Scalers -> state -> Effect (Maybe state)
```

It's invoked when a parent component calls `Halogen.tell` targeted at the canvas component.

## Halogen Component Output

When an application's state changes, an `OutputProducer` function is called:

```purescript
type OutputProducer state output = Delta -> Scalers -> Compare state -> Effect (Maybe output)
```

It's similar to an update function, with two main differences:

- Instead of a single state, it gets a `Compare state` record:

  ```purescript
  type Compare a = { old :: a, new :: a }
  ```

- Instead of returning a `Maybe state`, it returns a `Maybe output`

It has an opportunity to compare the two states and determine if the parent component needs to know about the difference. `Nothing` return values are ignored, while `Just` values lead to calling `Halogen.raise`.

This requires the `Slot` in the parent component to designate an `Action` to handle the output.
