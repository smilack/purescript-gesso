-- | AspectRatio can be used with a width or height to specify a `Size` in the
-- | `Dimensions` module. For example, if you want to target 16:9 screens but
-- | use 360 vertical units in your coordinate system (which divides evenly
-- | into 720 and 1080), you could specify your `Size` as 640 wide by 360 high
-- | or as 360 high with 16:9 ratio.
module Gesso.AspectRatio
  ( AspectRatio
  , toNumber
  , width
  , height
  , custom
  , w16h9
  , w16h10
  , w2h1
  , w4h3
  , w1h1
  ) where

import Prelude

-- | The `AspectRatio` type follows the normal convention of width:height,
-- | so `AspectRatio 16.0 9.0` is 16:9.
data AspectRatio
  = AspectRatio Number Number

instance showAspectRatio :: Show AspectRatio where
  show = show <<< toNumber

derive instance eqAspectRatio :: Eq AspectRatio

-- | Convert an `AspectRatio` to a single number suitable for scaling another
-- | number.
toNumber :: AspectRatio -> Number
toNumber (AspectRatio w h) = w / h

-- | Get the width given a height and aspect ratio
width :: Number -> AspectRatio -> Number
width forHeight (AspectRatio w h) = forHeight * w / h

-- | Get the height given a width and aspect ratio
height :: Number -> AspectRatio -> Number
height forWidth (AspectRatio w h) = forWidth * h / w

-- | Although several common ratios are provided, sometimes an arbitrary ratio
-- | is needed. `custom` creates an `AspectRatio` from any width and height.
custom :: Number -> Number -> AspectRatio
custom w h = AspectRatio w h

-- | 16:9, for example, high-definition television, 720p, 1080p, 4K, etc.
w16h9 :: AspectRatio
w16h9 = custom 16.0 9.0

w16h10 :: AspectRatio
w16h10 = custom 16.0 10.0

-- | 2:1, between cinema (2.39:1) and 16:9. For example, some TV shows and
-- | phones.
w2h1 :: AspectRatio
w2h1 = custom 2.0 1.0

-- | 4:3, for example, standard-definition televsion and 1024x768 monitors.
w4h3 :: AspectRatio
w4h3 = custom 4.0 3.0

-- | 1:1 - square - equal width and height.
w1h1 :: AspectRatio
w1h1 = custom 1.0 1.0
