module Gesso.Geometry.Internal (Scalers, mkScalers) where

import Prelude

import Gesso.Geometry.Dimensions (Rect, largestContainedArea)
import Gesso.Geometry.Scaler (Scaler, mkScaler)

-- | Data and functions for converting between the coordinate systems of the
-- | canvas element on the page and the view box of the application/drawing.
-- |
-- | `scale` is the amount that the view box has been scaled up or down to fit
-- | within the canvas element.
-- |
-- | `canvas` and `drawing` contain the dimensions of the named space and
-- | functions for converting coordinates to itself.
type Scalers =
  { scale :: Number
  , canvas :: Scaler
  , drawing :: Scaler
  }

-- | Create a `Scalers` record based on the view box of the application and the
-- | client rect ([`Gesso.Canvas.Element.getCanvasClientRect`](Gesso.Canvas.Element.html#v:getCanvasClientRect),
-- | [MDN: DOMRect](https://developer.mozilla.org/en-US/docs/Web/API/DOMRect))
-- | of the canvas.
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
