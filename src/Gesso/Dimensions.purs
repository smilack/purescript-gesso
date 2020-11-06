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
import Record (union)
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR
import Halogen.HTML.Properties as HP
import Math as Math
import Web.HTML.HTMLElement (DOMRect)
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
showSized s = "{ width: " <> width <> ", height: " <> height <> ", aspectRatio: " <> ar <> " }"
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
toSizeProps ::
  forall a i r.
  Sized a =>
  a -> Array (HP.IProp ( width :: HP.CSSPixel, height :: HP.CSSPixel | r ) i)
toSizeProps sized =
  [ HP.width $ round $ getWidth sized
  , HP.height $ round $ getHeight sized
  ]

-- | A class representing anything that is both `Sized` and `Positioned`.
class (Positioned a, Sized a) <= Dimensioned a

showDimensioned :: forall a. Dimensioned a => a -> String
showDimensioned d = "{ x: " <> x <> ", y: " <> y <> ", width: " <> width <> ", height: " <> height <> ", aspectRatio: " <> ar <> " }"
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
type WH
  = { width :: Number, height :: Number }

-- | A shorthand for a record with `width` and `aspectRatio` fields.
type WR
  = { width :: Number, aspectRatio :: AspectRatio }

-- | A shorthand for a record with `height` and `aspectRatio` fields.
type HR
  = { height :: Number, aspectRatio :: AspectRatio }

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
newtype Point
  = Point P

-- | A shorthand for a record with `x` and `y` fields.
type P
  = { x :: Number, y :: Number }

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
data Dimensions a
  = Dimensions Point Size

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
type ClientRect
  = Dimensions ClientRect'

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
type ViewBox
  = Dimensions ViewBox'

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
-- | The top level `x`, `y`, `w`, `h` functions convert from `ViewBox` to
-- | `ClientRect`, with the prime (`'`) functions returning the exact conversion
-- | and the underscore (`_`) functions returning a truncated number. For
-- | example, if `x' 1.0` returns `2.5` then `x_ 1.0` will return `2.0`.
-- |
-- | The `x` and `y` functions account for any margin that may exist (if the
-- | `ViewBox` and `ClientRect` are not the same size), while the `w` and `h`
-- | functions just scale.
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
-- | `toVb` contains functions for converting a page coordinate to a `ViewBox`
-- | coordinate, for example, for converting mouse position to drawing position.
type Scaler
  = { x' :: Number -> Number
    , y' :: Number -> Number
    , w' :: Number -> Number
    , h' :: Number -> Number
    , x_ :: Number -> Number
    , y_ :: Number -> Number
    , w_ :: Number -> Number
    , h_ :: Number -> Number
    , scale :: Number
    , viewBox ::
        { x :: Number
        , y :: Number
        , width :: Number
        , height :: Number
        }
    , screen ::
        { x :: Number
        , y :: Number
        , width :: Number
        , height :: Number
        }
    , toVb ::
        { x' :: Number -> Number
        , y' :: Number -> Number
        , x_ :: Number -> Number
        , y_ :: Number -> Number
        }
    }

-- | Creates a [`Scaler`](#t:Scaler) from a [`ViewBox`](#t:ViewBox) and
-- | [`ClientRect`](#t:ClientRect).
mkScaler :: ViewBox -> ClientRect -> Scaler
mkScaler viewBox clientRect =
  { x'
  , y'
  , w'
  , h'
  , x_: Math.round <<< x'
  , y_: Math.round <<< y'
  , w_: Math.round <<< w'
  , h_: Math.round <<< h'
  , scale: c
  , viewBox:
      { x: getX viewBox
      , y: getY viewBox
      , width: getWidth viewBox
      , height: getHeight viewBox
      }
  , screen:
      { x: 0.0
      , y: 0.0
      , width: getWidth clientRect
      , height: getHeight clientRect
      }
  , toVb: union toVb' toVb_
  }
  where
  actualVB = largestContainedArea (getRatio viewBox) clientRect

  margin =
    { w: (getWidth clientRect - getWidth actualVB) / 2.0
    , h: (getHeight clientRect - getHeight actualVB) / 2.0
    }

  c = getWidth viewBox / getWidth actualVB

  x' = (_ - w' $ getX viewBox) <<< (_ + margin.w) <<< w'

  w' = (_ / c)

  y' = (_ - h' $ getY viewBox) <<< (_ + margin.h) <<< h'

  h' = (_ / c)

  toVb' =
    { x': (_ + getX viewBox) <<< (_ * c) <<< (_ - getX clientRect + margin.w)
    , y': (_ + getY viewBox) <<< (_ * c) <<< (_ - getY clientRect + margin.h)
    }

  toVb_ =
    { x_: Math.round <<< toVb'.x'
    , y_: Math.round <<< toVb'.y'
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
p720 = fromPointAndSize origin $ fromHeightAndRatio { height: 720.0, aspectRatio: AR.w16h9 }

-- | A 1920x1080 `ViewBox` at (0, 0).
p1080 :: ViewBox
p1080 = fromPointAndSize origin $ fromHeightAndRatio { height: 1080.0, aspectRatio: AR.w16h9 }
