# Gesso Quick-Start Guide
These are the basic steps to get a brand new Gesso project up and running.

## 1. Initialize a new PureScript project
```bash
$ spago init
```

## 2. Install Gesso and purescript-canvas
```bash
$ spago install canvas
$ spago install gesso
```

> [!NOTE]  
> If gesso isn't in your package set, add these lines to the `extraPackages` section in `spago.yaml`:
> ```yaml
>   extraPackages:
>     gesso:
>       git: https://github.com/smilack/purescript-gesso.git
>       ref: master
> ```

## 3. Add necessary imports in `Main.purs`
```purescript
module Main where

import Prelude

import Effect (Effect)
import Gesso (launch)
import Gesso.Application (AppSpec, WindowMode(..), defaultBehavior)
import Gesso.Geometry (null)
import Graphics.Canvas (fillText)
```

## 4. Specify application settings
```purescript
appSpec :: forall i o. AppSpec Unit i o
appSpec =
  { name: "app"
  , initialState: unit
  , window: Fullscreen
  , viewBox: null
  , behavior: defaultBehavior
      { render = \context _ _ _ -> fillText context "hello world" 20.0 20.0
      }
  }
```

## 5. Add `main` function
```purescript
main :: Effect Unit
main = launch appSpec
```

## 6. Build project
```bash
$ spago build
```

## 7. Bundle project
```bash
$ spago bundle
```

> [!NOTE]  
> `spago bundle` uses `esbuild`. If you don't have it, you can install it with `npm`:
> ```bash
> $ npm install -g esbuild
> ```

## 8. Create basic HTML file
```html
<!doctype html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Gesso Quick-Start</title>
</head>
<body>
  <script src="./index.js"></script>
</body>
</html>
```

## 9. Open the file in a browser
You should the the words "hello world" in the upper left corner. Next, check out [the examples](../examples/README.md) to see what you can do with Gesso!
