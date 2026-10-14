# WebGL

This example sets up a WebGL canvas context and renders an animation of a rotating, color-changing triangle.

### Configuration

This example has two main differences from previous examples in its `behavior` record:

```purescript
, behavior: nominalBehavior
    { render = render
    , init = init
    , getContext = getDefaultWebGL
    }
```

First, it overrides `getContext`, which is the function that Gesso uses to get the rendering context (see [HTMLCanvasElement: getContext() method](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext)). `Gesso.Canvas.Context` exports functions for Context2D, WebGL, WebGL2, and WebGPU, both with and without the second argument (`contextAttributes`).

Second, the behavior is based on the `nominalBehavior` record instead of `defaultBehavior`. `defaultBehavior` has `getContext` set to `getDefault2D`, which constrains the types of the `render` and `init` functions to `Context2D -> ...`. `nominalBehavior`'s `getContext` function returns `Nothing`, which keeps the context unconstrained:

```purescript
type AppBehavior state input output context =
  { render :: RenderFunction context state
  , init :: InitFunction context state
  -- ...
  , getContext :: String -> Effect (Maybe context)
  }

defaultBehavior :: forall state input output. AppBehavior state input output Context2D

nominalBehavior :: forall state input output context. AppBehavior state input output context
```

In this case, `defaultBehavior` would still work because `render` and `init` are both changed, but if the updated `behavior` record were missing either of those functions, then the context types wouldn't unify.

### `init`

This example uses an `init` function to set up WebGL globals and compile shaders. The return type is unusual here because `init` returns an `Effect (Maybe state)` (since not all `init`s need to update the state directly), and the state in this example is a `Maybe { ... }`:

```purescript
init :: GL -> Delta -> Scalers -> State -> Effect (Maybe State)
init gl _ _ _ = do
  vertexShader <- createShader GL.vertexShader vertexShaderSource
  fragmentShader <- createShader GL.fragmentShader fragmentShaderSource
  program <- createProgram [ vertexShader, fragmentShader ]

  -- ...

  pure $ Just (Just { program, positionAttrs, positionBuffer, uColor, uAngle })
```

The `gl` instance doesn't need to be part of the state because it's passed as an argument to every render function.

## Sample output

[See this example in action]()

![A triangle on a white background. The triangle gradually changes color as it rotates.](webgl.webm)
