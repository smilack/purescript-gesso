module Gesso.Geometry.Internal
  ( Scalers
  , ZoneScalers
  , mkScalers
  , mkZoneScalers
  , mkReferenceFrame
  ) where

import Prelude

import Gesso.Geometry.Dimensions (Rect, largestContainedArea)
import Gesso.Geometry.Scaler (Scaler, mkScaler)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil)
import Record (get)
import Record.Builder (buildFromScratch, insert)
import Type.Prelude (class IsSymbol, Proxy(..))

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

-- |
type ZoneScalers =
  { scale ::
      { x :: Number
      , y :: Number
      }
  , a :: Scaler
  , b :: Scaler
  }

-- | Create a transformation between a region of a Scaler and a new coordinate frame
mkReferenceFrame
  :: forall parent child rowIn rowOut parRow
   . IsSymbol parent
  => IsSymbol child
  => Cons parent Rect () rowIn
  => Cons child Rect () rowIn
  => Cons parent Scaler () parRow
  => Cons child Scaler parRow rowOut
  => Lacks child parRow
  => RowToList rowIn (Cons child Rect (Cons parent Rect Nil))
  => RowToList rowOut (Cons child Scaler (Cons parent Scaler Nil))
  => Record rowIn
  -> Record rowOut
mkReferenceFrame a = out
  where
  parent = get (Proxy @parent) a
  child = get (Proxy @child) a
  { a, b } = mkZoneScalers parent child

  out :: Record rowOut
  out = buildFromScratch $ insert (Proxy @child) b <<< insert (Proxy @parent) a

-- |
mkZoneScalers :: Rect -> Rect -> ZoneScalers
mkZoneScalers a b =
  { scale
  , a: mkScaler b toA
  , b: mkScaler a toB
  }
  where
  scale =
    { x: b.width / a.width
    , y: b.height / a.height
    }

  toB =
    { x: (_ - a.x) >>> mul scale.x >>> add b.x
    , y: (_ - a.y) >>> mul scale.y >>> add b.y
    , length: mul scale.x
    }

  toA =
    { x: (_ - b.x) >>> (_ / scale.x) >>> add a.x
    , y: (_ - b.y) >>> (_ / scale.y) >>> add a.y
    , length: (_ / scale.x)
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
