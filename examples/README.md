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
npm run example-clock
npm run example-unit-grid
npm run example-paint
```

This creates an `example.js` file in the example's `dist` folder. Open `examples/{name}/dist/index.html` in your browser to see the example running.

## Examples Available

See each example's `README` for more detail and an explanation of its implementation:

- [Hello](https://github.com/smilack/purescript-gesso/tree/master/examples/hello) is the most minimal example that renders anything
- [Clock](https://github.com/smilack/purescript-gesso/tree/master/examples/clock) is a more complex drawing using the scaling functions that renders the current system time as an analog clock
- [Unit Grid](https://github.com/smilack/purescript-gesso/tree/master/examples/unit-grid) adds mouse events to show where the user clicked on a grid
- [Paint](https://github.com/smilack/purescript-gesso/tree/master/examples/paint) is a Halogen application with an embedded Gesso component. It uses Halogen queries to communicate between the Gesso component and parent component.
