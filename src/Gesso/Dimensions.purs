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
  , Point
  , fromXAndY
  , Dimensions
  , fromPointAndSize
  , ClientRect
  , fromDOMRect
  , origin
  , sizeless
  , null
  ) where

import Prelude
import CSS as CSS
import Data.Int (round)
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (DOMRect)

-----------------
-- Typeclasses --
-----------------
class Positioned a where
  getX :: a -> Number
  getY :: a -> Number

toPositionCss :: forall a. Positioned a => a -> CSS.CSS
toPositionCss positioned = do
  CSS.left $ CSS.px $ getX positioned
  CSS.top $ CSS.px $ getY positioned

class Sized a where
  getWidth :: a -> Number
  getHeight :: a -> Number
  getRatio :: a -> AspectRatio

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

---------------
-- Size Type --
---------------
data Size
  = WidthAndHeight WH
  | WidthAndRatio WR

type WH
  = { width :: Number, height :: Number }

type WR
  = { width :: Number, aspectRatio :: AspectRatio }

fromWidthAndHeight :: WH -> Size
fromWidthAndHeight = WidthAndHeight

fromWidthAndRatio :: WR -> Size
fromWidthAndRatio = WidthAndRatio

instance sizedSize :: Sized Size where
  getWidth = case _ of
    WidthAndHeight { width } -> width
    WidthAndRatio { width } -> width
  getHeight = case _ of
    WidthAndHeight { height } -> height
    WidthAndRatio { width, aspectRatio } -> AR.height width aspectRatio
  getRatio = case _ of
    WidthAndHeight { width, height } -> AR.custom width height
    WidthAndRatio { aspectRatio } -> aspectRatio

----------------
-- Point type --
----------------
newtype Point
  = Point P

type P
  = { x :: Number, y :: Number }

fromXAndY :: P -> Point
fromXAndY = Point

instance positionedPoint :: Positioned Point where
  getX (Point { x }) = x
  getY (Point { y }) = y

---------------------
-- Dimensions type --
---------------------
data Dimensions
  = Dimensions Point Size

instance positionedDimensions :: Positioned Dimensions where
  getX (Dimensions p _) = getX p
  getY (Dimensions p _) = getY p

instance sizedDimensions :: Sized Dimensions where
  getWidth (Dimensions _ s) = getWidth s
  getHeight (Dimensions _ s) = getHeight s
  getRatio (Dimensions _ s) = getRatio s

instance dimensionedDimensions :: Dimensioned Dimensions

fromPointAndSize :: Point -> Size -> Dimensions
fromPointAndSize = Dimensions

---------------------
-- ClientRect type --
---------------------
data ClientRect
  = ClientRect Point Size

instance positionedClientRect :: Positioned ClientRect where
  getX (ClientRect p _) = getX p
  getY (ClientRect p _) = getY p

instance sizedClientRect :: Sized ClientRect where
  getWidth (ClientRect _ s) = getWidth s
  getHeight (ClientRect _ s) = getHeight s
  getRatio (ClientRect _ s) = getRatio s

instance dimensionedClientRect :: Dimensioned ClientRect

fromDOMRect :: DOMRect -> ClientRect
fromDOMRect { left, top, width, height } =
  ClientRect
    (fromXAndY { x: left, y: top })
    (fromWidthAndHeight { width, height })

---------------
-- Constants --
---------------
origin :: Point
origin = fromXAndY { x: 0.0, y: 0.0 }

sizeless :: Size
sizeless = fromWidthAndHeight { width: 0.0, height: 0.0 }

null :: Dimensions
null = Dimensions origin sizeless
