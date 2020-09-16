module Gesso.Dimensions
  ( Dimensions
  , fromWidthAndHeight
  , fromWidthAndRatio
  , getWidth
  , getHeight
  , getRatio
  , getRecord
  ) where

import Prelude ((<$>), (<*>))
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR

data Dimensions
  = WidthAndHeight WH
  | WidthAndRatio WR

type WH
  = { width :: Number, height :: Number }

type WR
  = { width :: Number, aspectRatio :: AspectRatio }

fromWidthAndHeight :: Number -> Number -> Dimensions
fromWidthAndHeight width height = WidthAndHeight { width, height }

fromWidthAndRatio :: Number -> AspectRatio -> Dimensions
fromWidthAndRatio width aspectRatio = WidthAndRatio { width, aspectRatio }

getWidth :: Dimensions -> Number
getWidth = case _ of
  WidthAndHeight { width } -> width
  WidthAndRatio { width } -> width

getHeight :: Dimensions -> Number
getHeight = case _ of
  WidthAndHeight { height } -> height
  WidthAndRatio { width, aspectRatio } -> AR.height width aspectRatio

getRatio :: Dimensions -> AspectRatio
getRatio = case _ of
  WidthAndHeight { width, height } -> AR.custom width height
  WidthAndRatio { aspectRatio } -> aspectRatio

getRecord :: Dimensions -> { width :: Number, height :: Number }
getRecord = { width: _, height: _ } <$> getWidth <*> getHeight
