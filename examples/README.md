# Gesso Examples

This folder contains several complete examples of Gesso programs that show how to use the various options.

## Compiling Examples

Clone the Gesso repository to your computer. In the root of the repository, install the dependencies (`spago` and `purescript`) with:

```
npm install
```

Then, you can build each example (or all of them) with npm scripts:

```
npm run example-all

npm run example-hello
npm run example-bouncing-ball
npm run example-controlling-ball
npm run example-clock
npm run example-unit-grid
npm run example-paint
```

This creates an `example.js` file in the example's `dist` folder. Open `examples/{name}/dist/index.html` in your browser to see the example running.

## Examples Available

See each example's `README` for more detail and an explanation of its implementation:

- [Hello](hello) is the most minimal example that renders anything
- [Clock](clock) is a more complex drawing using the scaling functions that renders the current system time as an analog clock
- [Unit Grid](unit-grid) adds mouse events to show where the user clicked on a grid
- [Paint](paint) is a Halogen application with an embedded Gesso component. It uses Halogen queries to communicate between the Gesso component and parent component.
- [Bouncing Ball](bouncing-ball) - a circle that moves around the drawing and bounces off the edges.
- [Controlling the Ball](controlling-ball) - move the circle with the mouse or arrow keys.
