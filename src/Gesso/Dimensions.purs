-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions
  ( class Positioned
  , getX
  , getY
  , toPositionCss
  , class Sized
  , getWidth
  , getHeight
  , getRatio
  , toSizeCss
  , toSizeProps
  , class Dimensioned
  , Size
  , fromWidthAndHeight
  , fromWidthAndRatio
  , fromHeightAndRatio
  , Point
  , fromXAndY
  , fromMouseEvent
  , Dimensions
  , ClientRect
  , ClientRect'
  , fromDOMRect
  , ViewBox
  , ViewBox'
  , fromPointAndSize
  , Scaler
  , mkScaler
  , origin
  , sizeless
  , null
  , p720
  , p1080
  ) where

import Prelude
import CSS as CSS
import Data.Int (round, toNumber)
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR
import Gesso.Scale as Gesso.Scale
import Graphics.Canvas (Rectangle)
import Halogen.HTML.Properties as HP
import Web.DOM.Element (DOMRect)
import Web.UIEvent.MouseEvent (pageX, pageY, MouseEvent)

-----------------
-- Typeclasses --
-----------------
-- | A class representing anything with an (x, y) position.
class Positioned a where
  getX :: a -> Number
  getY :: a -> Number

showPositioned :: forall a. Positioned a => a -> String
showPositioned p = "{ x: " <> x <> ", y: " <> y <> " }"
  where
  x = show $ getX p

  y = show $ getY p

-- | Convert a `Positioned` item to `left` and `top` CSS attributes.
toPositionCss :: forall a. Positioned a => a -> CSS.CSS
toPositionCss positioned = do
  CSS.left $ CSS.px $ getX positioned
  CSS.top $ CSS.px $ getY positioned

-- | A class representing anything with a width and height.
class Sized a where
  getWidth :: a -> Number
  getHeight :: a -> Number
  getRatio :: a -> AspectRatio

showSized :: forall a. Sized a => a -> String
showSized s =
  "{ width: "
    <> width
    <> ", height: "
    <> height
    <> ", aspectRatio: "
    <> ar
    <> " }"
  where
  width = show $ getWidth s

  height = show $ getHeight s

  ar = show $ getRatio s

-- | Convert a `Sized` item to `width` and `height` CSS attributes.
toSizeCss :: forall a. Sized a => a -> CSS.CSS
toSizeCss sized = do
  CSS.width $ CSS.px $ getWidth sized
  CSS.height $ CSS.px $ getHeight sized

-- | Convert a `Sized` item to `width` and `height` HTML properties.
toSizeProps
  :: forall a i r
   . Sized a
  => a
  -> Array (HP.IProp (width :: HP.CSSPixel, height :: HP.CSSPixel | r) i)
toSizeProps sized =
  [ HP.width $ round $ getWidth sized
  , HP.height $ round $ getHeight sized
  ]

-- | A class representing anything that is both `Sized` and `Positioned`.
class (Positioned a, Sized a) <= Dimensioned a

showDimensioned :: forall a. Dimensioned a => a -> String
showDimensioned d =
  "{ x: "
    <> x
    <> ", y: "
    <> y
    <> ", width: "
    <> width
    <> ", height: "
    <> height
    <> ", aspectRatio: "
    <> ar
    <> " }"
  where
  x = show $ getX d

  y = show $ getY d

  width = show $ getWidth d

  height = show $ getHeight d

  ar = show $ getRatio d

---------------
-- Size Type --
---------------
-- | A `Size` can be constructed from any two of `width`, `height`, and
-- | `aspectRatio`.
data Size
  = WidthAndHeight WH
  | WidthAndRatio WR
  | HeightAndRatio HR

-- | A shorthand for a record with `width` and `height` fields.
type WH = { width :: Number, height :: Number }

-- | A shorthand for a record with `width` and `aspectRatio` fields.
type WR = { width :: Number, aspectRatio :: AspectRatio }

-- | A shorthand for a record with `height` and `aspectRatio` fields.
type HR = { height :: Number, aspectRatio :: AspectRatio }

-- | Construct a side from `width` and `height`.
fromWidthAndHeight :: WH -> Size
fromWidthAndHeight = WidthAndHeight

-- | Construct a side from `width` and `aspectRatio`.
fromWidthAndRatio :: WR -> Size
fromWidthAndRatio = WidthAndRatio

-- | Construct a side from `height` and `aspectRatio`.
fromHeightAndRatio :: HR -> Size
fromHeightAndRatio = HeightAndRatio

-- | Find the largest rectangle with the given aspect ratio that can fit
-- | inside the given client rectangle. The client rectangle will be the actual
-- | size of the drawing element, and the resulting `Size` is the size of the
-- | drawing area available to the program. There will usually be a margin as
-- | well. The [`Scaler`](#t:Scaler) type provides functions for dealing with
-- | the drawing area and margin.
largestContainedArea :: AspectRatio -> ClientRect -> Size
largestContainedArea aspectRatio clientRect = go
  where
  go
    | getHeight keepWidth <= getHeight clientRect = keepWidth
    | otherwise = keepHeight

  width = getWidth clientRect

  height = getHeight clientRect

  keepWidth = fromWidthAndRatio { width, aspectRatio }

  keepHeight = fromHeightAndRatio { height, aspectRatio }

instance sizedSize :: Sized Size where
  getWidth = case _ of
    WidthAndHeight { width } -> width
    WidthAndRatio { width } -> width
    HeightAndRatio { height, aspectRatio } -> AR.width height aspectRatio
  getHeight = case _ of
    WidthAndHeight { height } -> height
    WidthAndRatio { width, aspectRatio } -> AR.height width aspectRatio
    HeightAndRatio { height } -> height
  getRatio = case _ of
    WidthAndHeight { width, height } -> AR.custom width height
    WidthAndRatio { aspectRatio } -> aspectRatio
    HeightAndRatio { aspectRatio } -> aspectRatio

instance showSize :: Show Size where
  show = ("Size " <> _) <<< showSized

derive instance eqSize :: Eq Size

----------------
-- Point type --
----------------
-- | A `Point` is constructed from an `x` and a `y` value.
newtype Point = Point P

-- | A shorthand for a record with `x` and `y` fields.
type P = { x :: Number, y :: Number }

-- | Construct a `Point` from `x` and `y` values.
fromXAndY :: P -> Point
fromXAndY = Point

-- | Convert a `MouseEvent` to a `Point` by extracting the `pageX` and `pageY`
-- | values. The [`Scaler`](#t:Scaler) type provides functions for further
-- | converting page coordinates to drawing coordinates.
fromMouseEvent :: MouseEvent -> Point
fromMouseEvent me =
  fromXAndY
    { x: toNumber $ pageX $ me
    , y: toNumber $ pageY $ me
    }

instance positionedPoint :: Positioned Point where
  getX (Point { x }) = x
  getY (Point { y }) = y

instance showPoint :: Show Point where
  show = ("Point " <> _) <<< showPositioned

derive instance eqPoint :: Eq Point

---------------------
-- Dimensions type --
---------------------
-- | `Dimensions` types contain a `Point` and a `Size`. They also have a phantom
-- | type to identify where the dimensions came from.
-- | [`ClientRect`](#t:ClientRect) and [`ViewBox`](#t:ViewBox) are the two kinds
-- | of dimensions.
data Dimensions :: Type -> Type
data Dimensions a = Dimensions Point Size

instance positionedDimensions :: Positioned (Dimensions a) where
  getX (Dimensions p _) = getX p
  getY (Dimensions p _) = getY p

instance sizedDimensions :: Sized (Dimensions a) where
  getWidth (Dimensions _ s) = getWidth s
  getHeight (Dimensions _ s) = getHeight s
  getRatio (Dimensions _ s) = getRatio s

instance dimensionedDimensions :: Dimensioned (Dimensions a)

derive instance eqDimensions :: Eq (Dimensions a)

---------------------
-- ClientRect type --
---------------------
-- | A phantom type for tagging [`Dimensions`](#t:Dimensions) as a
-- | [`ClientRect`](#t:ClientRect).
data ClientRect'

-- | `ClientRect` represents the actual size and position of an HTML element.
type ClientRect = Dimensions ClientRect'

instance showClientRect :: Show (Dimensions ClientRect') where
  show = ("ClientRect " <> _) <<< showDimensioned

-- | Create a [`ClientRect`](#t:ClientRect) from a `DOMRect` (from
-- | `Web.HTML.HTMLElement`).
fromDOMRect :: DOMRect -> ClientRect
fromDOMRect { left, top, width, height } =
  Dimensions
    (fromXAndY { x: left, y: top })
    (fromWidthAndHeight { width, height })

------------------
-- ViewBox type --
------------------
-- | A phantom type for tagging [`Dimensions`](#t:Dimensions) as a
-- | [`ClientRect`](#t:ClientRect) and [`ViewBox`](#t:ViewBox)
data ViewBox'

-- | `ViewBox` represents the desired drawing area for an application.
type ViewBox = Dimensions ViewBox'

instance showViewBox :: Show (Dimensions ViewBox') where
  show = ("ViewBox " <> _) <<< showDimensioned

-- | Create a [`ViewBox`](#t:ViewBox) from a `Point` and `Size`
fromPointAndSize :: Point -> Size -> ViewBox
fromPointAndSize = Dimensions

-----------------
-- Scaler type --
-----------------
-- | A `Scaler` is given to interaction handlers and the render function to
-- | convert [`ViewBox`](#t:ViewBox) coordinates to
-- | [`ClientRect`](#t:ClientRect) coordinates and screen coordinates to
-- | `ViewBox` coordinates.
-- |
-- | This is important for fullscreen or stretched applications where the size
-- | of the canvas element may not be the same size as the chosen `ViewBox`.
-- |
-- | `scale` is the ratio of `ViewBox` to `ClientRect` units.
-- |
-- | `viewBox` contains the user-provided dimensions of the view box, unscaled,
-- | which are useful for clearing the drawing region, creating a border, or
-- | checking if something may have gone off-screen.
-- |
-- | `screen` contains the coordinates of the entire drawing element, in
-- | `ClientRect` units, with (0, 0) at the top left. This can be used to clear
-- | the entire drawing, draw an extended border, or check that something is
-- | completely off-screen.
-- |
-- | The `x`, `y`, `width`, `height`, `point`, `size`, and `dims` fields each
-- | contain two functions: one for converting from `ClientRect` to `ViewBox`
-- | units (`toVb`), and one for converting from `ViewBox` to `ClientRect`
-- | units (`toCr`).
-- |
-- | Typically the `toCr` functions will be more useful, as they allow you to
-- | specify units in the same coordinate system as the chosen `ViewBox`, and
-- | then they will handle the scaling and margins for you.
-- |
-- | The `toVb` functions may be useful in the context of certain events, for
-- | example, for converting the position of a mouse click.
-- |
-- | `toRectangle` is a helper that converts from a `Dimensioned` type to the
-- | `Rectangle` type from `Graphics.Canvas`.
type Scaler =
  { scale :: Number
  , viewBox :: ViewBox
  , screen :: ClientRect
  , x ::
      { toVb :: Number -> Number
      , toCr :: Number -> Number
      }
  , y ::
      { toVb :: Number -> Number
      , toCr :: Number -> Number
      }
  , width ::
      { toVb :: Number -> Number
      , toCr :: Number -> Number
      }
  , height ::
      { toVb :: Number -> Number
      , toCr :: Number -> Number
      }
  , point ::
      { toVb :: forall p. Positioned p => p -> Point
      , toCr :: forall p. Positioned p => p -> Point
      }
  , size ::
      { toVb :: forall s. Sized s => s -> Size
      , toCr :: forall s. Sized s => s -> Size
      }
  , dims ::
      { toVb :: ClientRect -> ViewBox
      , toCr :: ViewBox -> ClientRect
      }
  , rect ::
      { toVb :: Rectangle -> Rectangle
      , toCr :: Rectangle -> Rectangle
      }
  , toRectangle :: forall d. Dimensioned d => d -> Rectangle
  , scaler ::
      { page :: forall r. Gesso.Scale.Scaler Gesso.Scale.Page (Gesso.Scale.Rectangular' Number r)
      , drawing :: forall r. Gesso.Scale.Scaler Gesso.Scale.Drawing (Gesso.Scale.Rectangular' Number r)
      }
  }

-- | Creates a [`Scaler`](#t:Scaler) from a [`ViewBox`](#t:ViewBox) and
-- | [`ClientRect`](#t:ClientRect).
mkScaler :: ViewBox -> ClientRect -> Scaler
mkScaler viewBox clientRect@(Dimensions _ crSize) =
  { scale: c
  , viewBox
  , screen: Dimensions origin crSize
  , x:
      { toVb: toVb.x'
      , toCr: x'
      }
  , y:
      { toVb: toVb.y'
      , toCr: y'
      }
  , width:
      { toVb: toVb.w'
      , toCr: w'
      }
  , height:
      { toVb: toVb.h'
      , toCr: h'
      }
  , point:
      { toVb: transformP toVb.x' toVb.y'
      , toCr: transformP x' y'
      }
  , size:
      { toVb: transformS toVb.w' toVb.h'
      , toCr: transformS w' h'
      }
  , dims:
      { toVb: transformD toVb.x' toVb.y' toVb.w' toVb.h'
      , toCr: transformD x' y' w' h'
      }
  , rect:
      { toVb: transformR toVb.x' toVb.y' toVb.w' toVb.h'
      , toCr: transformR x' y' w' h'
      }
  , toRectangle
  , scaler:
      { page: Gesso.Scale.mkScalers @Gesso.Scale.Page
          { x: x'
          , y: y'
          , width: w'
          , height: h'
          }
      , drawing: Gesso.Scale.mkScalers @Gesso.Scale.Drawing
          { x: toVb.x'
          , y: toVb.y'
          , width: toVb.w'
          , height: toVb.h'
          }
      }
  }
  where
  actualVB = largestContainedArea (getRatio viewBox) clientRect

  margin =
    { w: (getWidth clientRect - getWidth actualVB) / 2.0
    , h: (getHeight clientRect - getHeight actualVB) / 2.0
    }

  c = getWidth viewBox / getWidth actualVB

  x' = (_ - (w' $ getX viewBox)) <<< (_ + margin.w) <<< w'

  w' = (_ / c)

  y' = (_ - (h' $ getY viewBox)) <<< (_ + margin.h) <<< h'

  h' = (_ / c)

  toVb =
    { x': (_ + getX viewBox) <<< (_ * c) <<< (_ - (getX clientRect + margin.w))
    , y': (_ + getY viewBox) <<< (_ * c) <<< (_ - (getY clientRect + margin.h))
    , w': (_ * c)
    , h': (_ * c)
    }

  transformP
    :: forall p
     . Positioned p
    => (Number -> Number)
    -> (Number -> Number)
    -> p
    -> Point
  transformP tx ty p =
    fromXAndY
      { x: tx $ getX p
      , y: ty $ getY p
      }

  transformS
    :: forall s
     . Sized s
    => (Number -> Number)
    -> (Number -> Number)
    -> s
    -> Size
  transformS tw th s =
    fromWidthAndHeight
      { width: tw $ getWidth s
      , height: th $ getHeight s
      }

  transformD
    :: forall a d
     . Dimensioned d
    => (Number -> Number)
    -> (Number -> Number)
    -> (Number -> Number)
    -> (Number -> Number)
    -> d
    -> Dimensions a
  transformD tx ty tw th d =
    Dimensions
      ( fromXAndY
          { x: tx $ getX d
          , y: ty $ getY d
          }
      )
      ( fromWidthAndHeight
          { width: tw $ getWidth d
          , height: th $ getHeight d
          }
      )

  transformR
    :: (Number -> Number)
    -> (Number -> Number)
    -> (Number -> Number)
    -> (Number -> Number)
    -> Rectangle
    -> Rectangle
  transformR tx ty tw th { x, y, width, height } =
    { x: tx x
    , y: ty y
    , width: tw width
    , height: th height
    }

  toRectangle :: forall d. Dimensioned d => d -> Rectangle
  toRectangle d =
    { x: getX d
    , y: getY d
    , width: getWidth d
    , height: getHeight d
    }

---------------
-- Constants --
---------------
-- | A `Point` at (0, 0).
origin :: Point
origin = fromXAndY { x: 0.0, y: 0.0 }

-- | A `Size` with no width or height.
sizeless :: Size
sizeless = fromWidthAndHeight { width: 0.0, height: 0.0 }

-- | A `ViewBox` at (0, 0) with no size.
null :: ViewBox
null = Dimensions origin sizeless

-- | A 1280x720 `ViewBox` at (0, 0).
p720 :: ViewBox
p720 =
  fromPointAndSize origin
    $ fromHeightAndRatio { height: 720.0, aspectRatio: AR.w16h9 }

-- | A 1920x1080 `ViewBox` at (0, 0).
p1080 :: ViewBox
p1080 =
  fromPointAndSize origin
    $ fromHeightAndRatio { height: 1080.0, aspectRatio: AR.w16h9 }
