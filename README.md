# PureScript Gesso

Gesso is a PureScript library that makes it easy to use `<canvas>` graphics in standalone applications or Halogen components.

Gesso (pronounced [['dʒɛsoʊ]](https://en.wikipedia.org/wiki/Help:IPA/English), like **jes**ter and espr**esso**) is named after a primer used to prepare canvas for painting.

### What does it do?

Gesso is designed to get you drawing on a `<canvas>` element with PureScript as quickly and easily as possible:

```purescript
module Main where

import Prelude
import Effect (Effect)
import Gesso (launch)
import Gesso.Application (AppSpec, WindowMode(..), defaultBehavior)
import Gesso.Geometry (null)
import Graphics.Canvas (fillText)

main :: Effect Unit
main = launch
  { name: "app"
  , initialState: unit
  , window: Fullscreen
  , viewBox: null
  , behavior: defaultBehavior
      { render = \context _ _ _ -> fillText context "hello world" 20.0 20.0
      }
  }
```

Check out the [Quick-Start Guide](docs/quickstart.md) to get set up right away.

Or, for more details, see [How does it work?](#how-does-it-work) below or the [documentation](docs).

## Installation

Gesso isn't in the PureScript Registry yet. To add it to your project, add these lines to the `extraPackages` section in your `spago.yaml`:

```yaml
  extraPackages:
    gesso:
      git: https://github.com/smilack/purescript-gesso.git
      ref: master
```

Then you can run `spago install gesso` as usual.

## How does it work?

You provide Gesso with:

- An **element** to run inside
- The **initial application state**
- A **view box** - origin, width, and height — like an SVG
- The **canvas size** - fullscreen, fixed size, or stretch to fit another element
- And any or all of these functions:
  - **render** - draw on the canvas, runs on every animation frame
  - **update** - make changes to the application state, runs immediately before rendering
  - **fixed** update - like update, but runs at a regular (configurable) time interval
  - **interactions** - any canvas event handlers, like mouse, keyboard, or touch events
  - **output** and **input** - communication with a parent Halogen component

Then, Gesso:

- Creates a `<canvas>` element and adds it to the page along with all of your event handlers
- Begins requesting animation frames
- Tracks youy application state through updates, I/O, and events
- Provides information about animation frame timing
- Provides functions to scale between view box coordinates and page coordinates
- Automatically accounts for window resizing

### Is this a...

- **Canvas API?** No, while Gesso gives you easy access to a `Context2D` object, it is agnostic about the way you interact with it. You could use the official canvas bindings in [purescript-canvas](https://pursuit.purescript.org/packages/purescript-canvas), another library with higher-level bindings, or your own custom ones. In fact, the original idea for Gesso was to simplify experimenting with custom canvas bindings.
- **Game engine?** No, Gesso does not provide anything like a physics engine, asset pipeline, or audio functions that a complete game engine might include. However, because Gesso uses `requestAnimationFrame` to run your update and rendering functions, you could certainly make a game with Gesso if you wanted to mix and match with other libraries or write your own handling for physics, sound, etc.

## Documentation

- [Quick-Start Guide](docs/quickstart.md)
- The [docs folder](docs/README.md) covers the various modules and the important types and functions in each
- There are a variety of [examples](examples/README.md) available to look through
- If you encounter a bug, the documentation is unclear or incorrect, or you have ideas for improving the API, open an issue.
- For general help or questions, create a thread on the [PureScript Discourse instance](https://discourse.purescript.org/) or the [PureScript Discord Server](https://purescript.org/chat).

## License

Gesso is licensed under the [MIT License](./LICENSE)
