module Gesso.Util.Lerp
  ( History
  , Lerp
  , LinearInterpolation
  , Versions
  , Versions'
  , lerp
  ) where

import Prelude

type Versions' a r = { old :: a, new :: a | r }

type Versions a = Versions' a ()

type History a = Versions' a (original :: a)

type LinearInterpolation a n = Versions' a (t :: n)

type Lerp a = LinearInterpolation a Number

lerp :: forall a. Ring a => LinearInterpolation a a -> a
lerp { old, new, t } = (one - t) * old + t * new

