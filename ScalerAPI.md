# Scaler API

## Coordinate spaces

### Page

Key coordinates:

- Upper left extreme: `(0, 0)` (constant)
- Bottom right extreme: `(x, y)` (varies as window is resized)

Potential names:

- page
- layout viewport
- visual viewport
- screen
  - could be confusing because it's not relative to the monitor
- `clientRect`
  - the name of the function to get the element's page coordinates
- `DOMRect`
  - the actual type of the `clientRect`

The `ClientRect` type is maybe conflated with screen coordinates? It makes sense for `ClientRect` to represent the dimensions of the canvas element, but I don't think it should be a term for *any* pagespace coordinate.

### ViewBox

User-supplied, with arbitrary "origin" and size. Origin might not be `(0, 0)`. For `Fullscreen` apps, origin will likely not be the upper left corner of the page or the canvas, although it should share at least an `x` or `y` coordinate with the canvas.

Potential names:

- `ViewBox`
  - same format as SVG `viewBox`
- viewport
- drawing
- canvas
- app

## Data types

(Subject to change)

```purescript
class Positioned a where
  getX :: a -> Number
  getY :: a -> Number

class Sized a where
  getWidth :: a -> Number
  getHeight :: a -> Number
  getRatio :: a -> AspectRatio

class (Positioned a, Sized a) <= Dimensioned a

type Rectangle =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }

data Size
  = WidthAndHeight WH
  | WidthAndRatio WR
  | HeightAndRatio HR
type WH = { width :: Number, height :: Number }
type WR = { width :: Number, aspectRatio :: AspectRatio }
type HR = { height :: Number, aspectRatio :: AspectRatio }

newtype Point = Point P
type P = { x :: Number, y :: Number }

data Dimensions a = Dimensions Point Size
```

## Values

- scale factor between drawing and page
- dimensions of drawing
- dimensions of page

## Operations

View box/drawing ⇌ page/viewport/client rect:

- coordinate part (x or y)
- coordinate/point (x, y pair)
- single dimension (width or height)
- size (width and height)
- dimensions (tagged point and size value)
- rectangle (untagged point and size record)

Convert dimensions to rectangle
