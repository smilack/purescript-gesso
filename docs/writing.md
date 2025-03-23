# Writing a Gesso program

## 1. Launching

The `Gesso` module ([`src/Gesso.purs`](../src/Gesso.purs)) contains functions for launching a Gesso application. Launching a Gesso application always requires an `AppSpec`, which will be [covered later](#2-appspec).

> [!TIP]
> If the Gesso component is going to be part of a larger Halogen application, this module isn't necessary. In that case, the `Canvas` module's `Slot` type will be used instead.

### `launch` and `launchIn`

These are the simplest options, designed for applications which require no `Aff` effects other than Gesso.

```purescript
launch :: forall state i o. AppSpec state i o -> Effect Unit

main :: Effect Unit
main = launch appSpec
```

`launch` is perfect for applications with nothing else on the page. Gesso attaches directly to the page body.

```purescript
launchIn :: forall state i o. String -> AppSpec state i o -> Effect Unit

main :: Effect Unit
main = launchIn "#some-element-id" appSpec
```

`launchIn` is best for pages with some static content. It takes a `String` as an argument, which is treated as a query selector to find an element on the page to attach to.

### `runGessoAff`

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

## 2. `AppSpec`

In `Gesso.Application` ([`src/Gesso/Application.purs`](../src/Gesso/Application.purs)), the `AppSpec` and `AppBehavior` types contain everything that make an application work.

```purescript
type AppSpec state input output =
  { name :: String
  , initialState :: state
  , viewBox :: Rect
  , window :: WindowMode
  , behavior :: AppBehavior state input output
  }
```

### Basics

The `state` type is the state of your application and can be anything you want, for example, a large complicated record, a single integer, or just `unit` if you don't need to track state at all

The `input` and `output` types are only used for communication with a parent component in a Halogen application, which is covered later.

The `name` field will be used as the `id` attribute for the canvas element.

> [!WARNING]
> If you plan to target the element with any CSS or JavaScript outside of what Gesso normally does, then it's best to make `name` a valid CSS identifier. Otherwise, it doesn't matter much.

### Canvas dimensions

`viewBox` is a [`Geometry.Rect`](TODO) that determines the coordinate system of the drawing. It is analagous to the `viewBox` attribute on an SVG - neither is tied to the actual size of the element on the page. This simplifies the drawing process when the screen size is unpredictable and subject to change.

`window` determines the space that the canvas element takes up:
- `Fixed` creates an element with an exact size.
- `Stretch` causes the element to fill its parent.
- `FullScreen` takes up the entire page from the top left corner to the bottom right.

The `viewBox` scales automatically to fit within the canvas element while remaining centered. Unless the view box and drawing have the exact same aspect ratio, this leaves a margin in the canvas outside of the view box on one axis. (That is, it behaves like SVG's `preserveAspectRatio="xMidYMid meet"`.)

## `AppBehavior`

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

All of these functions run in `Effect` and therefore have access to any `Effect`.

There are several very important types in the arguments to these functions. They'll be covered in more detail later, but for a quick rundown:

- `Delta`: the timestamps of the current and previous animation frames and the difference between them
- `Scalers`: information about the sizes of the canvas and the drawing (view box) and functions to convert between the two
- `States`: two sequential states (the current and previous) and the progress (on the interval `[0, 1]`) from the first to second
- `Compare`: two states - an old and new - not necessarily sequential

### Rendering

```purescript
type RenderFunction state = Context2D -> Delta -> Scalers -> States state -> Effect Unit
```

`render` is the only behavior function that has access to the canvas's `Context2D` for drawing.

The `States` record contains the current and previous states and an interpolation parameter. Typically, only the current is needed, and the interpolation parameter is `1`.

However, if there is a fixed-interval update function running at a different rate than the rendering function, the interpolation parameter will be a number in the range `[0, 1]` representing the progress from the previous state to the current. This can be used to smooth animations in some circumstances.

### Updates, component input, and events

Because of the overlap between kinds of state-changing functions, they use a few type synonyms to maintain consistency. This is not how they are literally written in the code, but it may be the most clear presentation:

```purescript
type UpdateFunction state = Delta -> Scalers -> local -> Effect (Maybe local)

type InputReceiver state input = input -> UpdateFunction state

-- Gesso.Interactions
type Handler event state = event -> UpdateFunction state
```

All kinds of state-changing functions have access to the same `Delta` and `Scalers` records as `render`, but only one state.

All of them also return a `Maybe state`, with a `Nothing` value indicating that no change was made.

### `update` vs `fixed`

`update` is the most basic kind of update function. It runs once per frame immediately before `render`.

`fixed` updates have a time interval in milliseconds (constructed using the `Gesso.Time.hz` function) and an update function. Gesso tracks the last time the fixed update function ran. Each frame, if the amount of time since then is greater than the interval, the fixed update is run repeatedly, with the timestamp and delta set according to the interval, until the most recent update has caught up to the interval. The `delta` in the `Delta` record *should* equal the interval, but there may be fluctuations if the fixed update function regularly runs for more time than the interval allows.

### Interactions


### Input

## TODO. Geometry module
