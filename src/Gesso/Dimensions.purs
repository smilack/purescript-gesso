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
  , fromMouseEvent
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
import Data.Int (round, toNumber)
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (DOMRect)
import Web.UIEvent.MouseEvent (screenX, screenY, MouseEvent)

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
    { x: toNumber $ screenX $ me
    , y: toNumber $ screenY $ me
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

instance showDimensions :: Show Dimensions where
  show = ("Dimensions " <> _) <<< showDimensioned

derive instance eqDimensions :: Eq Dimensions

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

instance showClientRect :: Show ClientRect where
  show = ("ClientRect " <> _) <<< showDimensioned

derive instance eqClientRect :: Eq ClientRect

---------------
-- Constants --
---------------
origin :: Point
origin = fromXAndY { x: 0.0, y: 0.0 }

sizeless :: Size
sizeless = fromWidthAndHeight { width: 0.0, height: 0.0 }

null :: Dimensions
null = Dimensions origin sizeless
