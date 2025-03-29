# PureScript Gesso

Gesso is a PureScript library that makes it simple to use `<canvas>` graphics in standalone applications or Halogen components.

Pronounced like **jes**ter and espr**esso** ([/'dʒɛsoʊ/](https://en.wikipedia.org/wiki/Help:IPA/English)), Gesso is named after a primer used to prepare canvas for painting.

This is all it takes to start drawing on a `<canvas>`:

```purescript
module Main where

import Prelude
import Effect (Effect)
import Gesso (launch)
import Gesso.Application (WindowMode(..), defaultBehavior)
import Gesso.Geometry (null)
import Graphics.Canvas (fillText)

main :: Effect Unit
main = launch
  { name: "app"
  , initialState: unit
  , window: Fullscreen
  , viewBox: null
  , behavior: defaultBehavior
      { render = \context _ _ _ -> fillText context "hello world" 20.0 20.0 }
  }
```

To get started right away, check out the [Quick-Start Guide](docs/quickstart.md).

## How does it work?

You tell Gesso:

- Which element to put the canvas in

- The initial state of your application

- The size of the canvas: fullscreen, fixed size, or fill container

- Optionally, a separate viewport for the drawing (like [`svg:viewBox`](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/viewBox))

- Any of these functions:

<table>
 <tbody>
  <tr>
   <td>
Render
   </td>
   <td>
Draw on the canvas on each animation frame
   </td>
  </tr>
  <tr>
   <td>
Update
   </td>
   <td>
Make changes to the application state immediately before rendering
   </td>
  </tr>
  <tr>
   <td>
Fixed update
   </td>
   <td>
Make changes to the state at a regular, configurable time interval
   </td>
  </tr>
  <tr>
   <td>
Interactions
   </td>
   <td>
Event handlers, like mouse, keyboard, or touch events
   </td>
  </tr>
  <tr>
   <td>
Input and output
   </td>
   <td>
Communication with a parent Halogen component
   </td>
  </tr>
 </tbody>
</table>

Then, Gesso:

Creates a `<canvas>` element, adds it to the page, sets its size and position, attaches event handlers, and starts requesting animation frames to call your rendering function. It tracks your application state and runs your update functions. It can react to queries from other Halogen components and send output when your state changes. It provides timestamps and delta times to all your functions. When using a fixed-rate update, it provides interpolation information to your render function. It provides dimensions for the `<canvas>` and your drawing, and functions for scaling coordinates and sizes between the two, while automatically accounting for changes to the page size.

## Is Gesso a...

### Canvas API?
No, while Gesso gives you easy access to a `Context2D` object, it's agnostic about the way you interact with it. You could use the basic canvas bindings in [purescript-canvas](https://pursuit.purescript.org/packages/purescript-canvas), another library with higher-level bindings, or your own custom ones. In fact, the original idea for Gesso was to simplify experimenting with custom canvas bindings.

### Game engine?
No, Gesso does not provide anything like a physics engine, asset pipeline, or audio functions that a complete game engine might include. However, because Gesso renders with `requestAnimationFrame` and supports both per-frame and fixed-interval update functions, you could certainly make a game with Gesso if you wanted to mix and match other libraries or write your own handling for physics, sound, etc.

## Installation

Install with [Spago](https://github.com/purescript/spago#installation):

```
spago install gesso
```

> [!NOTE]
> Gesso is available starting in package set 63.6.0. If you're using an earlier package set, add these lines to the `extraPackages` section in `spago.yaml`:
> ```yaml
>   extraPackages:
>     gesso:
>       git: https://github.com/smilack/purescript-gesso.git
>       ref: v1.0.0
> ```

## Documentation

- [Quick-Start Guide](docs/quickstart.md)
- [The Gesso Manual](docs/manual.md) covers the vast majority of what you need to know to use Gesso effectively.
- There are a variety of [examples](examples/README.md) available to look through.
- Details about specific functions and types can be found on [Pursuit](https://pursuit.purescript.org/packages/purescript-gesso/).
- If you encounter a bug, the documentation is unclear or incorrect, or you have ideas for improving the API, open an issue.
- For general help or questions, create a thread on the [PureScript Discourse instance](https://discourse.purescript.org/) or the [PureScript Discord Server](https://purescript.org/chat).

## License

Gesso is licensed under the [MIT License](./LICENSE)
