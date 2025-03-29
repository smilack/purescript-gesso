# Gesso Documentation

## Quick-Start

The [Quick-Start Guide](quickstart.md) walks you through the process of setting up and compiling your first Gesso application.

## Gesso Manual

[The Gesso Manual](manual.md) covers the vast majority of what you need to know to use Gesso effectively:

- How to launch a Gesso application
- App configuration
- The kinds of update functions
- Event handling
- Coordinate scaling
- Parameters of rendering, update, event, and I/O functions
- I/O with Halogen components

Details about specific functions and types can be found on [Pursuit](https://pursuit.purescript.org/packages/purescript-gesso/).

## Examples

These small programs showcase various features. See [Gesso Examples](../examples/README.md) for more information on building and running the examples.

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
