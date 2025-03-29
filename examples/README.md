# Gesso Examples

This folder contains several Gesso programs showcasing a variety of options. Each example's readme has more details and a link to a pre-compiled version you can run online.

| Name | Summary |
|-|-|
| [Hello](hello) | The most barebones Gesso program: exactly what you need to get a canvas to draw on, and nothing more. |
| [Bounce](bounce) | A circle that moves around the canvas and bounces off the edges. |
| [Keyboard](keyboard) | Move a square around the canvas using the arrow keys. |
| [Mouse And Scaling](mouse-and-scaling) | A graph that shows the coordinates of a mouse click. Scales coordinates between canvas and drawing. |
| [Analog Clock](analog-clock) | A more complex drawing. Uses coordinate scaling functions and non-canvas `Effect`s. |
| [Interpolation](interpolation) | Fixed-rate update functions and interpolating state for rendering. |
| [Paint App](paint-app) | A Halogen application with an embedded Gesso component. |
| [Timing](timing) | A graph comparing the `delta` values in fixed and per-frame update functions. |

## Compiling the examples

Here are the steps to compile and run them locally:

### Prerequisites

- [PureScript compiler](https://github.com/purescript/purescript)
- [Spago](https://github.com/purescript/spago#installation)
- [esbuild](https://esbuild.github.io/getting-started/)

The easiest way to get set up is to install these all globally with `npm`:

```
npm install -g purescript
npm install -g spago@next
npm install -g esbuild
```

### Building

First, clone the Gesso repository to your computer.

To run the examples locally, they have to be bundled. You can bundle a single example with this command:

```
spago bundle --source-maps -p gesso-example-hello
```

> [!NOTE]
> The `--source-maps` option lets you see PureScript code in the browser debugger.

Or, you can bundle all the examples at once using the `bundle-examples.sh` script, which runs that command for every example.

The example package names are:

```
gesso-example-hello
gesso-example-bounce
gesso-example-keyboard
gesso-example-mouse-and-scaling
gesso-example-analog-clock
gesso-example-interpolation
gesso-example-paint-app
gesso-example-timing
```

### Running

An `index.html` file is provided in each example's `dist` folder. For example, after bundling the `hello` example, you can open up `examples/hello/dist/index.html` in your browser to run it.
