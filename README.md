# Gesso

Gesso is a PureScript library that makes it easy to use `<canvas>` graphics in standalone applications or Halogen components.

Gesso (pronounced [['dʒɛsoʊ]](https://en.wikipedia.org/wiki/Help:IPA/English), like **jes**ter and espr**esso**) is named after a primer used to prepare canvas for painting.

### What does Gesso do?

You provide Gesso with:

- Initial application state
- Window mode (fullscreen, fixed size, or stretch to fit another element) and a view box (origin point, width, and height, like an SVG)
- Render function and whether to render continuously or only when the state changes
- Update function run before each render
- Event handlers
- (Optionally) I/O or global state functions to communicate with a parent application

Then, Gesso:

- Creates a `<canvas>` element, adds it to the page, and gets its `Context2D`
- Adds your event handlers to the element
- Tracks local state through updates, I/O, and events
- Updates and renders during animation frames
- Passes the current time and a viewport scaler to your functions
- Adjusts to window resizing

### Is Gesso a...

- **Canvas API?** No, while Gesso gives you easy access to the `Context2D` type, it is agnostic about the way you interact with it. You could use the official Canvas bindings in [purescript-canvas](https://pursuit.purescript.org/packages/purescript-canvas), another library with higher-level bindings, or your own custom ones.
- **Game engine?** No, Gesso does not provide anything like a physics engine, asset pipeline, or audio functions that a complete game engine might include. However, because Gesso uses `requestAnimationFrame` to run your update and rendering functions, you could certainly make a game with Gesso if you wanted to mix and match other libraries or write your own handling for physics, sound, etc.

## Installation

Hmm...

## Documentation

- Documentation, including a quick-start guide, is in the docs folder.
- If you encounter a bug, the documentation is unclear or incorrect, or you have ideas for improving the API, open an issue.
- For general help or questions, create a thread on the [PureScript Discourse](https://discourse.purescript.org/).

## Examples

There are several complete example programs in the examples folder:

- [Hello](https://github.com/smilack/purescript-gesso/tree/master/examples/hello) is the most minimal example that renders anything
- [Clock](https://github.com/smilack/purescript-gesso/tree/master/examples/clock) is a more complex drawing using the scaling functions that renders the current system time as an analog clock
- [Unit Grid](https://github.com/smilack/purescript-gesso/tree/master/examples/unit-grid) adds mouse events to show where the user clicked on a grid
- [Paint (I/O)](https://github.com/smilack/purescript-gesso/tree/master/examples/paint-io) and [Paint (Global)](https://github.com/smilack/purescript-gesso/tree/master/examples/paint-global) are both Halogen applications with an embedded Gesso component. One uses Halogen queries for communicating between the Gesso component and parent component, while the other uses a shared mutable global state.

## Contributing

## License

Gesso is licensed under...
