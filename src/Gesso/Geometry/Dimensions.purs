-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Geometry.Dimensions
  ( Align
  , Alignment(..)
  , Area
  , Box
  , Boxed
  , Point
  , Position
  , PreserveAspectRatio(..)
  , Rect
  , Rectangular
  , ReferenceFrame
  , Size
  , largestContainedArea
  , null
  , origin
  , preserveAspectRatio
  , sizeless
  ) where

import Prelude

import Record (union) as Record
import Type.Row (type (+))

-- | A row representing anything that can have `x` and `y` values.
type Position :: Type -> Row Type -> Row Type
type Position a r =
  ( x :: a
  , y :: a
  | r
  )

-- | An `(x, y)` coordinate
type Point :: Type
type Point = { | Position Number () }

-- | A row representing anything that can have a `width` and `height`.
type Size :: Type -> Row Type -> Row Type
type Size a r =
  ( width :: a
  , height :: a
  | r
  )

-- | The size of a rectangle.
type Area :: Type
type Area = { | Size Number () }

-- | A row representing anything that can have `x`, `y`, `width`, and `height`
-- | properties.
type Rectangular :: Type -> Row Type -> Row Type
type Rectangular a r = Position a + Size a + r

-- | A rectangle positioned in space.
type Rect :: Type
type Rect = { | Rectangular Number () }

-- | A row with the inset properties of a CSS positioned box.
type Boxed :: Type -> Row Type -> Row Type
type Boxed a r =
  ( top :: a
  , right :: a
  , bottom :: a
  , left :: a
  | r
  )

-- | A CSS positioned box.
type Box :: Type
type Box = { | Boxed Number () }

-- | Given the sizes of an inner and an outer rectangle, find the largest size
-- | the inner can be scaled to while still fitting entirely within the outer.
largestContainedArea
  :: forall r1 r2
   . { | Size Number r1 }
  -> { | Size Number r2 }
  -> Area
largestContainedArea drawing canvas = area
  where
  area
    | widthLimited.height <= canvas.height = widthLimited
    | otherwise = heightLimited

  ratio = drawing.width / drawing.height

  widthLimited =
    { width: canvas.width
    , height: canvas.width / ratio
    }

  heightLimited =
    { height: canvas.height
    , width: canvas.height * ratio
    }

mkRect :: Point -> Area -> Rect
mkRect = Record.union

-- | A `Point` at `(0.0, 0.0)`
origin :: Point
origin = { x: 0.0, y: 0.0 }

-- | An `Area` with no width or height
sizeless :: Area
sizeless = { width: 0.0, height: 0.0 }

-- | A `Rect` with no width or height, located at the origin.
null :: Rect
null = mkRect origin sizeless

-- | A mapping from one coordinate system to another. When used with `Rect`,
-- | defines coordinates of equal regions in both systems. Mostly equivalent to
-- | [`Scalers`](Gesso.Geometry.Internal#t:Scalers) when returned with `Scaler`
-- | by [`mkReferenceFrame`](Gesso.Geometry.Internal#v:mkReferenceFrame).
type ReferenceFrame :: Type -> Type
type ReferenceFrame a =
  { outer :: a
  , inner :: a
  }

-- | How a scaled item should be aligned on a single axis:
-- |
-- | - `Min`: Left or top
-- | - `Mid`: Centered
-- | - `Max`: Right or bottom
data Alignment :: Type
data Alignment = Min | Mid | Max

-- | How a scaled item should be aligned on each axis.
type Align :: Type
type Align = { | Position Alignment () }

-- | Defines the scaling and alignment of one area relative to another, using
-- | the same rules as the
-- | [`preserveAspectRatio` SVG attribute](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/preserveAspectRatio)
data PreserveAspectRatio :: Type
data PreserveAspectRatio = None | Meet Align | Slice Align

-- | Scale the `inner` `Rect` relative to `outer` using the
-- | `PreserveAspectRatio` rules given. The result is a `Rect` in the `outer`
-- | coordinate system's units describing the area of the scaled `inner`.
preserveAspectRatio :: PreserveAspectRatio -> ReferenceFrame Rect -> Rect
preserveAspectRatio par { outer, inner } = case par of
  None -> outer { x = 0.0, y = 0.0 }
  Meet { x, y } ->
    let
      area
        | widthScaled.height <= outer.height = widthScaled
        | otherwise = heightScaled
    in
      calculateRect x y area
  Slice { x, y } ->
    let
      area
        | widthScaled.height <= outer.height = heightScaled
        | otherwise = widthScaled
    in
      calculateRect x y area
  where
  aspectRatio = inner.width / inner.height

  widthScaled =
    { width: outer.width
    , height: outer.width / aspectRatio
    }

  heightScaled =
    { height: outer.height
    , width: outer.height * aspectRatio
    }

  calculateRect x y area =
    let
      margin =
        { width: outer.width - area.width
        , height: outer.height - area.height
        }
      position =
        { x: case x of
            Min -> 0.0
            Mid -> margin.width / 2.0
            Max -> margin.width
        , y: case y of
            Min -> 0.0
            Mid -> margin.height / 2.0
            Max -> margin.height
        }
    in
      mkRect position area
