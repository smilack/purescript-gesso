module Gesso.Geometry
  ( Scalers
  , mkScalers
  , module Exports
  ) where

import Prelude

import Gesso.Geometry.Dimensions (Rect, largestContainedArea)
import Gesso.Geometry.Dimensions (Area, Point, Position, Rect, Rectangular, Size, null, origin, sizeless) as Exports
import Gesso.Geometry.Scaler (Scaler, mkScaler)
import Gesso.Geometry.Scaler ((>>@), (@<~), (^^@), (~>@), (~~@), Scaler, from, lengthTo, to, xTo, yTo) as Exports

type Scalers =
  { scale :: Number
  , canvas :: Scaler
  , drawing :: Scaler
  }

mkScalers :: Rect -> Rect -> Scalers
mkScalers viewBox clientRect =
  { scale: k
  , drawing: mkScaler viewBox toDrawing
  , canvas: mkScaler
      clientRect { x = 0.0, y = 0.0 }
      toCanvas
  }
  where
  fullView = largestContainedArea viewBox clientRect

  margin =
    { width: (clientRect.width - fullView.width) / 2.0
    , height: (clientRect.height - fullView.height) / 2.0
    }

  k = viewBox.width / fullView.width

  toCanvas =
    { x: (_ / k) >>> add margin.width >>> (_ - (viewBox.x / k))
    , y: (_ / k) >>> add margin.height >>> (_ - (viewBox.y / k))
    , length: (_ / k)
    }

  toDrawing =
    { x: (_ - (clientRect.x + margin.width)) >>> mul k >>> add viewBox.x
    , y: (_ - (clientRect.y + margin.height)) >>> mul k >>> add viewBox.y
    , length: mul k
    }
