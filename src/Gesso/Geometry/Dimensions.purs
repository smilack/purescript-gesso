-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Geometry.Dimensions
  ( Align
  , Alignment
  , Area
  , Box
  , Boxed
  , Point
  , Position
  , PreserveAspectRatio
  , Rect
  , Rectangular
  , ReferenceFrame
  , Size
  , largestContainedArea
  , null
  , origin
  , sizeless
  ) where

import Prelude

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

-- | A `Point` at `(0.0, 0.0)`
origin :: Point
origin = { x: 0.0, y: 0.0 }

-- | An `Area` with no width or height
sizeless :: Area
sizeless = { width: 0.0, height: 0.0 }

-- | A `Rect` with no width or height, located at the origin.
null :: Rect
null = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

-- |
type ReferenceFrame a =
  { outer :: a
  , inner :: a
  }

-- |
data Alignment = Min | Mid | Max

type Align = { x :: Alignment, y :: Alignment }

data PreserveAspectRatio = None | Meet Align | Slice Align

-- | Adjust the
-- preserveAspectRatio :: PreserveAspectRatio -> ReferenceFrame Rect -> ReferenceFrame Rect
-- preserveAspectRatio par { outer, inner } = { outer, inner }
