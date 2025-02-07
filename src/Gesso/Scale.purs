module Gesso.Scale
  ( (@>)
  , (@>>)
  , (@@)
  , (@^)
  , (@^^)
  , CoordinateSystem(..)
  , Dimensioned
  , Dimensioned'
  , Drawing
  , Page
  , Rect
  , Rect'
  , Scaler
  , ScalingFunctions
  , mkScalers
  , scale
  , scaleHeight
  , scaleWidth
  , scaleX
  , scaleY
  ) where

import Prelude hiding (flip)

import Record.Builder (modify, build, Builder, flip, merge)
import Record.Extra (class Keys, pick)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Union, class Nub)
import Type.RowList (class RowToList)

-- ┌───────────────────────────┐
-- │ Dimension & scaling types │
-- └───────────────────────────┘

type Dimensioned' :: Type -> Row Type -> Row Type
type Dimensioned' a r =
  ( x :: a
  , y :: a
  , width :: a
  , height :: a
  | r
  )

type Dimensioned :: Type -> Row Type -> Type
type Dimensioned a r = { | Dimensioned' a r }

type Rect' :: Row Type
type Rect' = Dimensioned' Number ()

type Rect :: Type
type Rect = Record Rect'

type ScalingFunctions :: Row Type
type ScalingFunctions = Dimensioned' (Number -> Number) ()

data CoordinateSystem

foreign import data Page :: CoordinateSystem

foreign import data Drawing :: CoordinateSystem

type Scaler :: CoordinateSystem -> Row Type -> Type
type Scaler c r =
  { scaler :: Builder (Record r) (Record r)
  | ScalingFunctions
  }

-- ┌────────────────────┐
-- │ Scaling operations │
-- └────────────────────┘

defaults :: Rect
defaults = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

fill
  :: forall partial union complete
   . Union partial Rect' union
  => Nub union complete
  => Builder (Record partial) (Record complete)
fill = flip merge defaults

scale
  :: forall partial u complete keys filler c
   . Union partial Rect' u
  => Nub u complete
  => RowToList partial keys
  => Union partial filler complete
  => Keys keys
  => Record partial
  -> Scaler c complete
  -> Record partial
scale r { scaler } = pick $ build (scaler <<< fill) r

scaleX :: forall c r. Number -> Scaler c r -> Number
scaleX n { x } = x n

scaleY :: forall c r. Number -> Scaler c r -> Number
scaleY n { y } = y n

scaleWidth :: forall c r. Number -> Scaler c r -> Number
scaleWidth n { width } = width n

scaleHeight :: forall c r. Number -> Scaler c r -> Number
scaleHeight n { height } = height n

infix 2 scale as @@
infix 2 scaleX as @>
infix 2 scaleY as @^
infix 2 scaleWidth as @>>
infix 2 scaleHeight as @^^

mkScaler
  :: forall @l a t r
   . IsSymbol l
  => Cons l a t r
  => (a -> a)
  -> Builder (Record r) (Record r)
mkScaler = modify (Proxy @l)

mkScalers
  :: forall @c r
   . Record ScalingFunctions
  -> Scaler c (Dimensioned' Number r)
mkScalers { x, y, width, height } = { scaler, x, y, width, height }
  where
  scaler = mkScaler @"x" x
    <<< mkScaler @"y" y
    <<< mkScaler @"width" width
    <<< mkScaler @"height" height
