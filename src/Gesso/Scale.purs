module Gesso.Scale
  ( (@>)
  , (@>>)
  , (@@)
  , (@^)
  , (@^^)
  , CoordinateSystem(..)
  , Drawing
  , Page
  , Rect
  , Rect'
  , Rectangular
  , Rectangular'
  , Scaler
  , ScalingFunctions
  , heightTo
  , mkScalers
  , to
  , widthTo
  , xTo
  , yTo
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

type Rectangular' :: Type -> Row Type -> Row Type
type Rectangular' a r =
  ( x :: a
  , y :: a
  , width :: a
  , height :: a
  | r
  )

type Rectangular :: Type -> Row Type -> Type
type Rectangular a r = { | Rectangular' a r }

type Rect' :: Row Type
type Rect' = Rectangular' Number ()

type Rect :: Type
type Rect = Record Rect'

type ScalingFunctions :: Row Type
type ScalingFunctions = Rectangular' (Number -> Number) ()

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

to
  :: forall partial u complete keys filler c
   . Union partial Rect' u
  => Nub u complete
  => RowToList partial keys
  => Union partial filler complete
  => Keys keys
  => Record partial
  -> Scaler c complete
  -> Record partial
to r { scaler } = pick $ build (scaler <<< fill) r

xTo :: forall c r. Number -> Scaler c r -> Number
xTo n { x } = x n

yTo :: forall c r. Number -> Scaler c r -> Number
yTo n { y } = y n

widthTo :: forall c r. Number -> Scaler c r -> Number
widthTo n { width } = width n

heightTo :: forall c r. Number -> Scaler c r -> Number
heightTo n { height } = height n

infix 2 to as @@
infix 2 xTo as @>
infix 2 yTo as @^
infix 2 widthTo as @>>
infix 2 heightTo as @^^

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
  -> Scaler c (Rectangular' Number r)
mkScalers { x, y, width, height } = { scaler, x, y, width, height }
  where
  scaler = mkScaler @"x" x
    <<< mkScaler @"y" y
    <<< mkScaler @"width" width
    <<< mkScaler @"height" height
