# Quick Start: `Hello` Example

Hello is the most minimal example of a Gesso application. All imports except `Maybe`, `Effect`, and Prelude functions are fully qualified so you can start to get a feel for where each type comes from and how they fit together.

## Output

Because this example does not use the `Scaler` functions (introduced in other examples), your output may look different depending on the size of your browser window.

![Hello example output](/examples/hello/output.png)

## Complete Code

```purescript
module Example.Hello.Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gesso as Gesso
import Gesso.Application as Gesso.Application
import Gesso.Canvas as Gesso.Canvas
import Gesso.Dimensions as Gesso.Dimensions
import Gesso.Interactions as Gesso.Interactions
import Gesso.Time as Gesso.Time
import Graphics.Canvas as Graphics.Canvas
import Prelude (Unit, unit, bind, ($))

main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body

canvasInput :: forall g i o. Gesso.Canvas.Input Unit g i o
canvasInput =
  { name: "hello"
  , localState: unit
  , app:
      Gesso.Application.mkApplication
        $ Gesso.Application.defaultApp
            { window = Gesso.Application.fullscreen
            , render = Just $ Gesso.Application.continuous render
            }
  , viewBox: Gesso.Dimensions.p1080
  , interactions: Gesso.Interactions.default
  }

render :: Unit -> Gesso.Time.Delta -> Gesso.Dimensions.Scaler -> Graphics.Canvas.Context2D -> Effect Unit
render _ _ _ context = do
  Graphics.Canvas.fillText context "hello world" 500.0 500.0
```

## Explanation

### `main`

`main` for a standalone Gesso application looks similar to a simple Halogen application. 

```purescript
main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body
```

The first two lines are standard for a PureScript program:

```purescript
main :: Effect Unit
main =
```

`runGessoAff` is an alias for `runHalogenAff`. It allows us to write asynchronous code and run it as an `Effect`.

```purescript
  Gesso.runGessoAff do
```

Next we find the HTML `<body>` tag. For this program, we will use the whole `<body>`. In other programs we can use a different function to select a more specific element.

```purescript
    body <- Gesso.awaitBody
```

Finally, we tell Gesso to `run` a `canvas` component, using the `canvasInput` we will define next, and to put it in the `body` tag we just found.

```purescript
    Gesso.run Gesso.canvas canvasInput body
```

### `canvasInput`

The input to the canvas component tells it how big the drawing should be, what kind of state it should track, and how to render the state. More advanced tutorials will also add updating the state, and responding to input like mouse events.

```purescript
canvasInput :: forall g i o. Gesso.Canvas.Input Unit g i o
canvasInput =
  { name: "hello"
  , localState: unit
  , app:
      Gesso.Application.mkApplication
        $ Gesso.Application.defaultApp
            { window = Gesso.Application.fullscreen
            , render = Just $ Gesso.Application.continuous render
            }
  , viewBox: Gesso.Dimensions.p1080
  , interactions: Gesso.Interactions.default
  }
```

The canvas's `Input` type takes four type variables. The `localState` must always be set, but in this example there is no state, so we use the `Unit` type. The other variables are for interacting with Halogen controls outside of the canvas. Since this example is canvas-only, we can leave them open.

```purescript
canvasInput :: forall g i o. Gesso.Canvas.Input Unit g i o
canvasInput =
```

The `name` is a string that will be used as the `id` of the canvas element. If you have multiple canvases, you should use a different name for each. Aside from that, it can be any valid HTML `id`.

```purescript
  { name: "hello"
```

Because there is no state in this example, we use the `Unit` type and its only value, `unit`.

```purescript
  , localState: unit
```

The application parameter is where the render is where the render function and canvas element size are set. There are many parameters that will not be needed for every Gesso application, so the easiest way to create the application input is to start with the default.

Using [record update syntax](https://github.com/purescript/documentation/blob/master/language/Records.md#record-update), we can set just the parameters we need:

- This will be a `fullscreen` application. We could also make it a fixed size, or stretch to fill available space.
- It will render `continuously` (every animation frame) using the `render` function we define below.

```purescript
  , app:
      Gesso.Application.mkApplication
        $ Gesso.Application.defaultApp
            { window = Gesso.Application.fullscreen
            , render = Just $ Gesso.Application.continuous render
            }
```

`viewBox` defines the drawing's coordinate system. With a fullscreen or stretched canvas, the drawing will remain centered and scale until it is just big (or small) enough to be completely contained in the available space. Therefore, coordinates in the drawing may not be the same size as coordinates on the rest of the page. Gesso provides functions to convert between the two coordinate systems, which appear in more advanced tutorials.

The process is essentially the same as setting the `viewBox` on an SVG with [`preserveAspectRatio` set to `"xMidYMid meet"`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/preserveAspectRatio).

```purescript
  , viewBox: Gesso.Dimensions.p1080
```

`interactions` are JavaScript events such as mouse, touch, and keyboard events. This `default` value is "no event handlers."

```purescript
  , interactions: Gesso.Interactions.default
  }
```

### `render`

The `render` function has several inputs. In this example, we only need the canvas context, so the others are ignored with a `_` character.

Using the Canvas API bindings from [`purescript-canvas`](https://pursuit.purescript.org/packages/purescript-canvas/4.0.0), we print a message on our drawing. You could also use any other Canvas API bindings that work with the `Context2D` type from `Graphics.Canvas`.

```purescript
render :: Unit -> Gesso.Time.Delta -> Gesso.Dimensions.Scaler -> Graphics.Canvas.Context2D -> Effect Unit
render _ _ _ context = do
  Graphics.Canvas.fillText context "hello world" 500.0 500.0
```

## Next Steps

The next tutorial, [Bouncing Ball](https://github.com/smilack/purescript-gesso/blob/master/docs/tutorials/bouncing-ball.md), introduces application state and the update function.
