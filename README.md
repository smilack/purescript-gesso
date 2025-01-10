# Gesso

Gesso is a PureScript library that makes it easy to use `<canvas>` graphics in standalone applications or Halogen components.

Gesso (pronounced [['dʒɛsoʊ]](https://en.wikipedia.org/wiki/Help:IPA/English), like **jes**ter and espr**esso**) is named after a primer used to prepare canvas for painting.

### What does Gesso do?

Gesso is designed to get you drawing on a `<canvas>` element with PureScript as quickly and easily as possible.

You provide Gesso with:

- Render function
- Window mode (fullscreen, fixed size, or stretch to fit another element)
- View box (origin, width, and height — like an SVG)
- Initial application state
- Optionally:
  - Update function to run before each render
  - Event handlers
  - I/O functions to communicate with a parent application

Then, Gesso:

- Creates a `<canvas>` element, adds it to the page, and gets its `Context2D`
- Adds your event handlers to the element
- Tracks local state through updates, I/O, and events
- Updates and renders during animation frames
- Passes the current time and a viewport scaler to your functions
- Automatically accounts for window resizing

### Is Gesso a...

- **Canvas API?** No, while Gesso gives you easy access to a `Context2D` object, it is agnostic about the way you interact with it. You could use the official canvas bindings in [purescript-canvas](https://pursuit.purescript.org/packages/purescript-canvas), another library with higher-level bindings, or your own custom ones. In fact, the original idea for Gesso was to simplify experimenting with custom canvas bindings.
- **Game engine?** No, Gesso does not provide anything like a physics engine, asset pipeline, or audio functions that a complete game engine might include. However, because Gesso uses `requestAnimationFrame` to run your update and rendering functions, you could certainly make a game with Gesso if you wanted to mix and match with other libraries or write your own handling for physics, sound, etc.

## Installation

This library has not been published on Pursuit yet. To use it in your project, it must be added to your `packages.dhall`. The default `packages.dhall` looks something like this:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220523/packages.dhall
        sha256:985f90fa68fd8b43b14c777d6ec2c161c4dd9009563b6f51685a54e4a26bf8ff

in  upstream
```

On the line after `in upstream`, add this:

```dhall
with gesso =
  { repo = "https://github.com/smilack/purescript-gesso.git"
  , version = "v0.2-beta"
  , dependencies =
    [ "aff"
    , "canvas"
    , "console"
    , "css"
    , "dom-indexed"
    , "effect"
    , "foldable-traversable"
    , "halogen"
    , "halogen-subscriptions"
    , "integers"
    , "lists"
    , "maybe"
    , "prelude"
    , "safe-coerce"
    , "tuples"
    , "web-clipboard"
    , "web-dom"
    , "web-events"
    , "web-html"
    , "web-touchevents"
    , "web-uievents"
    ]
  }
```

Finally, run Spago's install command:

```console
$ spago install gesso
```

## Documentation

***The documentation is still in progress and some pages are incomplete.***

- Documentation, including a quick-start guide, is in the [docs](docs) folder.
- If you encounter a bug, the documentation is unclear or incorrect, or you have ideas for improving the API, open an issue.
- For general help or questions, create a thread on the [PureScript Discourse instance](https://discourse.purescript.org/) or the [PureScript Discord Server](https://purescript.org/chat).

## Examples

***The documentation is still in progress and some pages are incomplete.***

There are several example programs in the examples folder:

- [Hello](examples/hello) is the most minimal example that renders anything
- [Clock](examples/clock) is a more complex drawing using the scaling functions that renders the current system time as an analog clock
- [Unit Grid](examples/unit-grid) adds mouse events to show where the user clicked on a grid
- [Paint](examples/paint) is a Halogen application with an embedded Gesso component. It uses Halogen queries to communicate between the Gesso component and parent component.
- [Bouncing Ball](examples/bouncing-ball) - a circle that moves around the drawing and bounces off the edges.
- [Controlling the Ball](examples/controlling-ball) - move the circle with the mouse or arrow keys.

## Contributing

## License

Gesso is licensed under...
