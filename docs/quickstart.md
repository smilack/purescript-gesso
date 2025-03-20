# Gesso Quick-Start Guide
These are the basic steps to get a brand new Gesso project up and running.

## 1. Initialize a new PureScript project
```bash
$ spago init
```

## 2. Install Gesso and HTML Canvas
```bash
$ spago install canvas
$ spago install gesso
```

## 3. Add necessary imports in `Main.purs`
```purescript
import Prelude

import Effect (Effect)
import Gesso (awaitBody, canvas, run, runGessoAff)
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
main = runGessoAff do
  body <- awaitBody
  run canvas appSpec body
```

## 6. Build project
```bash
$ spago build
```

## 7. Bundle project
(`npm install -g esbuild`)