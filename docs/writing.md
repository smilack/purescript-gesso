# Writing a Gesso program

## 1. Launching

The `Gesso` module ([`src/Gesso.purs`](../src/Gesso.purs)) contains functions for launching a Gesso application. Launching a Gesso application always requires an `AppSpec`, which will be [covered later](#2-application-module).

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

## 2. Application module

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

## TODO. Geometry module
