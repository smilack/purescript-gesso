# Gesso

Gesso is a PureScript library that makes it easy to use `<canvas>` graphics in standalone applications or Halogen components.

Gesso (pronounced [['dʒɛsoʊ]](https://en.wikipedia.org/wiki/Help:IPA/English), like **jes**ter and espr**esso**) is named after a primer used to prepare canvas for painting.

### What does Gesso do?

You provide Gesso with:

- Initial application state
- Window mode (fullscreen, fixed size, or stretch to fit another element) and a view box (origin point, width, and height, like an SVG)
- Render function
- (Optionally) Update function run before each render
- (Optionally) Event handlers
- (Optionally) I/O functions to communicate with a parent application

Then, Gesso:

- Creates a `<canvas>` element, adds it to the page, and gets its `Context2D`
- Adds your event handlers to the element
- Tracks local state through updates, I/O, and events
- Updates and renders during animation frames
- Passes the current time and a viewport scaler to your functions
- Adjusts to window resizing

### Is Gesso a...

- **Canvas API?** No, while Gesso gives you easy access to the `Context2D` type, it is agnostic about the way you interact with it. You could use the official Canvas bindings in [purescript-canvas](https://pursuit.purescript.org/packages/purescript-canvas), another library with higher-level bindings, or your own custom ones.
- **Game engine?** No, Gesso does not provide anything like a physics engine, asset pipeline, or audio functions that a complete game engine might include. However, because Gesso uses `requestAnimationFrame` to run your update and rendering functions, you could certainly make a game with Gesso if you wanted to mix and match with other libraries or write your own handling for physics, sound, etc.

## Installation

This library has not been published on Pursuit yet. To use it in your project, it must be added to your `packages.dhall`. The default `packages.dhall` looks something like this:

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

in  upstream
```

On the line after `in upstream`, add this:

```dhall
with gesso =
  { repo = "https://github.com/smilack/purescript-gesso.git"
  , version = "v0.1-alpha"
  , dependencies =
    [ "aff-bus"
    , "canvas"
    , "console"
    , "debug"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "halogen-hooks"
    , "newtype"
    , "psci-support"
    , "record"
    ]
  }
with css.version = "e5beb6b16f14e7d4dc10a0bb930c936fe9dde88a"
with css.repo = "https://github.com/purescript-contrib/purescript-css.git"
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
