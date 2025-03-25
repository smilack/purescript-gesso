# Gesso Examples

This folder contains several examples of Gesso programs that show how to use the various options. Here's a quick description of them:

- [Hello](hello): the most barebones Gesso program. Exactly what you need to get a canvas to draw on, and nothing more.
- [Bounce](bounce): a circle that moves around the canvas and bounces off the edges.
- [Keyboard](keyboard): move a square around the canvas using the arrow keys.
- [Mouse And Scaling](mouse-and-scaling): a graph that shows the coordinates of a mouse click. Scales coordinates between page and drawing.
- [Analog Clock](analog-clock): a more complex drawing. Uses coordinate scaling functions and non-canvas `Effect`s.
- [Paint App](paint-app): a Halogen application with an embedded Gesso component.

Each example's readme has more details and a link to a pre-compiled version you can run online without downloading anything.

## Compiling the examples

Here are the steps to compile and run them locally:

### Prerequisites

- [PureScript compiler](https://github.com/purescript/purescript)
- [Spago](https://github.com/purescript/spago#installation)
- [esbuild](https://esbuild.github.io/getting-started/)

The easiest way to get set up is to install them all globally with `npm`:

```
npm install -g purescript
npm install -g spago@next
npm install -g esbuild
```

### Building

Clone the Gesso repository to your computer. You can build the library and all examples with `spago build`, or specify a single example, e.g.:

```
spago build -p gesso-example-hello
```

### Bundling

To run the examples locally, they have to be bundled. You can bundle a single example:

```
spago bundle --source-maps -p gesso-example-hello
```

or use the `bundle-examples.sh` script, which runs this command for every example package.

### Running

An `index.html` file is provided for each example in its `dist` folder. For example, after bundling the `hello` example, you can open up `examples/hello/dist/index.html` in your browser to run it.
