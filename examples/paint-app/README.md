# Paint App

This example shows how to use a Gesso component inside a Halogen application to make a small drawing app. It uses Halogen queries to communicate with the parent component.

### Structure

`Root` is the main component. It contains the Gesso component (`Grid`) and four button components (`ColorButton`). It keeps track of the color of the pixels in the drawing and the order that they changed colors, in order to provide undo and redo features.

`ColorButton` renders a button element for selecting a color to draw with. It emits an `Output` when clicked.

`Grid` wraps the Gesso canvas. It uses this `AppSpec`:

```purescript
  , behavior: GApp.defaultBehavior
      { render = renderApp
      , output = extractOutput
      , input = convertState
      , interactions
          { mouse = [ highlightCell, clearHighlight, mouseDown, mouseUp ]
          }
      }
```

When the canvas state changes, `extractOutput` is called with a `Gesso.State.Compare` record. If this returns a `Just` value, Gesso automatically calls `Halogen.raise` to send output to `Root`.

When `Root` sends a query to `Grid` with `Halogen.tell`, `Grid` reacts to the query with `convertState`.

### Layout

This example uses a view box from `0` to `32` in both axes to simplify coloring individual squares.

It also uses a fixed, 600 by 600 pixel size.

### Scaling operators

This example uses the operator aliases for scaling functions:

```purescript
x = floor $ point.x -~> drawing

y = floor $ point.y |~> drawing

Canvas.setLineWidth context $ 0.05 /~> canvas

Canvas.fillRect context $
  { x: toNumber x, y: toNumber y, width: 1.0, height: 1.0 } *~> canvas
```

## Output

[See this example in action](https://smilack.github.io/purescript-gesso/examples/paint/dist/)

![A small drawing application. On the left, there are four buttons to pick a shade of gray to draw with. On the right, there are undo and redo buttons and a list of colors and coordinates showing the order that pixels were given a color. At the bottom, a checkbox with the label "Show Grid" is checked. In the middle, a canvas divided into a 32 by 32 grid has the word "example" written on it in various shades of gray.](paint-app.png)
