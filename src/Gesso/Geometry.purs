module Gesso.Geometry
  ( fromMouseEvent
  , module Exports
  ) where

import Prelude

import Data.Int (toNumber)
import Gesso.Geometry.Dimensions (Area, Point, Position, Rect, Rectangular, Size, null, origin, sizeless) as Exports
import Gesso.Geometry.Dimensions (Point)
import Gesso.Geometry.Internal (Scalers) as Exports
import Gesso.Geometry.Scaler ((*~>), (-~>), (/~>), (<~*), (<~-), (<~/), (<~|), (|~>), Scaler, from, lengthFrom, lengthTo, to, xFrom, xTo, yFrom, yTo) as Exports
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (pageX, pageY) as MouseEvent

-- | Extract `x` and `y` coordinates from a `MouseEvent` using the
-- | [`pageX` and `pageY` properties](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/pageX)
fromMouseEvent :: MouseEvent -> Point
fromMouseEvent event =
  { x: toNumber $ MouseEvent.pageX event
  , y: toNumber $ MouseEvent.pageY event
  }
