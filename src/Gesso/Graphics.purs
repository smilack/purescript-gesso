module Gesso.Graphics
  ( Point
  , ViewBox
  , ClientRect
  , mkViewBox
  , mkClientRect
  , origin
  , getBoundingBox
  , class BoundingBox
  ) where

import Gesso.Dimensions as Dims
import Data.Newtype (class Newtype, unwrap)

type Point
  = { x :: Number, y :: Number }

origin :: Point
origin = { x: 0.0, y: 0.0 }

type BB
  = { x :: Number, y :: Number, w :: Number, h :: Number }

newtype ViewBox
  = ViewBox BB

derive instance newtypeViewBox :: Newtype ViewBox _

newtype ClientRect
  = ClientRect BB

derive instance newtypeClientRect :: Newtype ClientRect _

mkViewBox :: Point -> Dims.Dimensions -> ViewBox
mkViewBox { x, y } dimensions = ViewBox { x, y, w: width, h: height }
  where
  { width, height } = Dims.getRecord dimensions

mkClientRect :: Point -> Dims.Dimensions -> ClientRect
mkClientRect { x, y } dimensions = ClientRect { x, y, w: width, h: height }
  where
  { width, height } = Dims.getRecord dimensions

class BoundingBox a where
  getBoundingBox :: a -> BB

instance boundingBoxViewBox :: BoundingBox ViewBox where
  getBoundingBox = unwrap

instance boundingBoxClientRect :: BoundingBox ClientRect where
  getBoundingBox = unwrap
