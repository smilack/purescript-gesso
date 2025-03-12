-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Geometry.Dimensions
  ( Area
  , Point
  , Position
  , Rect
  , Rectangular
  , Size
  , largestContainedArea
  , null
  , origin
  , sizeless
  ) where

import Prelude

import Type.Row (type (+))

type Position :: Type -> Row Type -> Row Type
type Position a r =
  ( x :: a
  , y :: a
  | r
  )

type Point :: Type
type Point = { | Position Number () }

type Size :: Type -> Row Type -> Row Type
type Size a r =
  ( width :: a
  , height :: a
  | r
  )

type Area :: Type
type Area = { | Size Number () }

type Rectangular :: Type -> Row Type -> Row Type
type Rectangular a r = Position a + Size a + r

type Rect :: Type
type Rect = { | Rectangular Number () }

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

origin :: Point
origin = { x: 0.0, y: 0.0 }

sizeless :: Area
sizeless = { width: 0.0, height: 0.0 }

null :: Rect
null = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
