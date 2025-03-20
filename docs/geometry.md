# `Gesso.Geometry` module

## `Scaler` and `Scalers`

## Row types

`Geometry` contains three pairs of Row and Record types that may be useful. The
Row types are open and have a type parameter:

```purescript
type Position a r = ( x :: a, y :: a | r )

type Size a r = ( width :: a, height :: a | r )

type Rectangular a r = Position a + Size a + r
```

Each Row has a corresponding closed record with the type specified as `Number`:

```purescript
type Point = { | Position Number () }

type Area = { | Size Number () }

type Rect = { | Rectangular Number () }
```

## `fromMouseEvent`