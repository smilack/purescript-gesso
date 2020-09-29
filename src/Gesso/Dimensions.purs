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
class Positioned a where
  getX :: a -> Number
  getY :: a -> Number

showPositioned :: forall a. Positioned a => a -> String
showPositioned p = "{ x: " <> x <> ", y: " <> y <> " }"
  where
  x = show $ getX p

  y = show $ getY p

toPositionCss :: forall a. Positioned a => a -> CSS.CSS
toPositionCss positioned = do
  CSS.left $ CSS.px $ getX positioned
  CSS.top $ CSS.px $ getY positioned

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

toSizeCss :: forall a. Sized a => a -> CSS.CSS
toSizeCss sized = do
  CSS.width $ CSS.px $ getWidth sized
  CSS.height $ CSS.px $ getHeight sized

toSizeProps ::
  forall a i r.
  Sized a =>
  a -> Array (HP.IProp ( width :: HP.CSSPixel, height :: HP.CSSPixel | r ) i)
toSizeProps sized =
  [ HP.width $ round $ getWidth sized
  , HP.height $ round $ getHeight sized
  ]

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
data Size
  = WidthAndHeight WH
  | WidthAndRatio WR
  | HeightAndRatio HR

type WH
  = { width :: Number, height :: Number }

type WR
  = { width :: Number, aspectRatio :: AspectRatio }

type HR
  = { height :: Number, aspectRatio :: AspectRatio }

fromWidthAndHeight :: WH -> Size
fromWidthAndHeight = WidthAndHeight

fromWidthAndRatio :: WR -> Size
fromWidthAndRatio = WidthAndRatio

fromHeightAndRatio :: HR -> Size
fromHeightAndRatio = HeightAndRatio

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
newtype Point
  = Point P

type P
  = { x :: Number, y :: Number }

fromXAndY :: P -> Point
fromXAndY = Point

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
data ClientRect'

type ClientRect
  = Dimensions ClientRect'

instance showClientRect :: Show (Dimensions ClientRect') where
  show = ("ClientRect " <> _) <<< showDimensioned

fromDOMRect :: DOMRect -> ClientRect
fromDOMRect { left, top, width, height } =
  Dimensions
    (fromXAndY { x: left, y: top })
    (fromWidthAndHeight { width, height })

------------------
-- ViewBox type --
------------------
data ViewBox'

type ViewBox
  = Dimensions ViewBox'

instance showViewBox :: Show (Dimensions ViewBox') where
  show = ("ViewBox " <> _) <<< showDimensioned

fromPointAndSize :: Point -> Size -> ViewBox
fromPointAndSize = Dimensions

-----------------
-- Scaler type --
-----------------
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
origin :: Point
origin = fromXAndY { x: 0.0, y: 0.0 }

sizeless :: Size
sizeless = fromWidthAndHeight { width: 0.0, height: 0.0 }

null :: ViewBox
null = Dimensions origin sizeless

p720 :: ViewBox
p720 = fromPointAndSize origin $ fromHeightAndRatio { height: 720.0, aspectRatio: AR.w16h9 }

p1080 :: ViewBox
p1080 = fromPointAndSize origin $ fromHeightAndRatio { height: 1080.0, aspectRatio: AR.w16h9 }
