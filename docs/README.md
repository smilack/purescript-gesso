# Gesso Documentation

## Quick-Start

The [Quick-Start Guide](quickstart.md) walks you through the process of setting up and compiling your first Gesso application.

## Gesso Manual

[The Gesso Manual](manual.md) covers the vast majority of what you need to know to use Gesso effectively:

- The ways to launch a Gesso application
- All the fields of the application spec
- The kinds of update functions
- Event handlers
- Coordinate scaling
- All of the data available in rendering, update, event, and i/o functions
- Halogen component I/O

Details on usage of specific functions can be found on [Pursuit](TODO).

## Examples

These small programs show various features. See the [main Examples page](../examples/README.md) for more information on building an running the examples.

- [Hello](hello): is the most barebones Gesso program. Exactly what you need to get a canvas to draw on, and nothing more.
- [Bounce](bounce): a circle that moves around the canvas and bounces off the edges.
- [Keyboard](keyboard): move a square around the canvas using the arrow keys.
- [Mouse And Scaling](mouse-and-scaling): a graph that shows the coordinates of a mouse click. Scales coordinates between page and drawing.
- [Analog Clock](analog-clock): a more complex drawing. Uses coordinate scaling functions and non-canvas `Effect`.
- [Interpolation](interpolation): fixed-rate update functions and interpolating state for rendering.
- [Paint App](paint-app): a Halogen application with an embedded Gesso component.
