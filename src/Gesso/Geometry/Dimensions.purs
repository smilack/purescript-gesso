-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Geometry.Dimensions
  ( Point
  , Rect
  , Size
  , largestContainedArea
  , null
  , origin
  , sizeless
  ) where

import Prelude

import Type.Row (type (+))

type Point :: Type -> Row Type -> Row Type
type Point a r =
  ( x :: a
  , y :: a
  | r
  )

type Size :: Type -> Row Type -> Row Type
type Size a r =
  ( width :: a
  , height :: a
  | r
  )

type Rect :: Row Type
type Rect = Point Number + Size Number + ()

largestContainedArea
  :: forall r1 r2
   . { | Size Number r1 }
  -> { | Size Number r2 }
  -> { | Size Number () }
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

origin :: { | Point Number () }
origin = { x: 0.0, y: 0.0 }

sizeless :: { | Size Number () }
sizeless = { width: 0.0, height: 0.0 }

null :: { | Rect }
null = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }
