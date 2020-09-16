module Gesso.AspectRatio
  ( AspectRatio
  , toNumber
  , height
  , custom
  , w16h9
  , w16h10
  , w2h1
  , w4h3
  , w1h1
  ) where

import Prelude ((*), (/))

data AspectRatio
  = AspectRatio Number Number

toNumber :: AspectRatio -> Number
toNumber (AspectRatio w h) = w / h

height :: Number -> AspectRatio -> Number
height width (AspectRatio w h) = width * h / w

custom :: Number -> Number -> AspectRatio
custom w h = AspectRatio w h

w16h9 :: AspectRatio
w16h9 = custom 16.0 9.0

w16h10 :: AspectRatio
w16h10 = custom 16.0 10.0

w2h1 :: AspectRatio
w2h1 = custom 2.0 1.0

w4h3 :: AspectRatio
w4h3 = custom 4.0 3.0

w1h1 :: AspectRatio
w1h1 = custom 1.0 1.0
