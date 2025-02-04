-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions2 where

import Prelude

import CSS as CSS
import Data.Int (round, toNumber)
import Gesso.AspectRatio (AspectRatio)
import Gesso.AspectRatio as AR
import Graphics.Canvas (Rectangle)
import Halogen.HTML.Properties as HP
import Record (get, set, modify, insert)
import Type.Function (type ($), APPLY)
import Type.Proxy (Proxy(..))
import Type.Row (type (+), RowApply, class Cons)
import Type.RowList (RowList, Nil, Cons, class RowToList, class ListToRow)
import Web.DOM.Element (DOMRect)
import Web.UIEvent.MouseEvent (pageX, pageY, MouseEvent)

{- ┌───────┐
-- │ TLP?? │
-- └───────┘
--}

data Field :: Symbol -> Type
data Field key

class Convertible :: Symbol -> (RowList Type -> RowList Type) -> Constraint
class Convertible key list where
  convert
    :: forall
         (val :: Type)
         (tailr :: Row Type)
         (taill :: RowList Type)
         (row :: Row Type)
         (other :: Row Type -> Row Type)
     . RowToList tailr taill
    => RowToList row (Cons key val taill)
    => RowToList row (list taill)
    => Cons key val tailr row
    => (Number -> Number)
    -> { | other row }
    -> { | other row }

{- instance Convertible "x" (Cons "x" Number) where
convert
  :: forall tailr taill other row
   . RowToList tailr taill
  => RowToList row (Cons "x" Number taill)
  => Cons "x" Number tailr row
  => (Number -> Number)
  -> { | other (x :: Number | tailr) }
  -> { | other (x :: Number | tailr) }
convert _ r = r -}

asdf
  :: forall row list key val tailrow taillist
   . Cons "x" Number tailrow row
  -- =>
  => { x :: Number | tailrow }
  -> { x :: Number | tailrow }
asdf a = a { x = 1.0 }

-- type Conv :: (Row Type -> Row Type) -> Type
type Conv a = { | a } -> { | a }

type ConvMap =
  { x :: forall r. Conv (x :: Number | r)
  , y :: forall r. Conv (y :: Number | r)
  , w :: forall r. Conv (w :: Number | r)
  , h :: forall r. Conv (h :: Number | r)
  }

convmap :: ConvMap
convmap =
  { x: \a -> a { x = 0.0 }
  , y: \a -> a { y = 0.0 }
  , w: \a -> a { w = 0.0 }
  , h: \a -> a { h = 0.0 }
  }

-- newtype F :: Row Type -> Type
-- newtype F row = F (Record row)

newtype F :: Type -> Type
newtype F rec = F rec

class Convible :: (Type -> Type) -> Row Type -> Constraint
class Convible f row | f -> row where
  conv :: f { | row } -> f { | row }

instance Convible F (x :: Number | r) where
  conv
    :: F { x :: Number | r }
    -> F { x :: Number | r }
  conv (F rec) = F (rec { x = rec.x + 20.0 })

else instance Convible F (y :: Number | r) where
  conv
    :: F { y :: Number | r }
    -> F { y :: Number | r }
  conv (F rec) = F (rec { y = rec.y - 20.0 })

else instance Convible F r where
  conv = identity

aaa :: forall r. Convible F r => { | r } -> { | r }
aaa = F >>> conv >>> \(F rec) -> rec

{- Is it possible to write a function that:
- takes an arbitrary record with fields known at compile time
- updates certain fields if they exist
- does not modify any other fields
For example:
```haskell
bToZero { a: 1 } == { a: 1 }
bToZero { b: 2 } == { b: 0 }
bToZero { a: 1, c: 3 } == { a: 1, c: 3 }
bToZero { a: 1, b: 2, c: 3 } == { a: 1, b: 0, c: 3 }
```
I've been attempting this for long enough that I need to check if it can be done before I keep going :joy:  -}

-- instance Convible F (headr (x :: Number | tailr)) where
--   conv
--     :: F { | headr (x :: Number | tailr) }
--     -> F { | headr (x :: Number | tailr) }
--   conv (F f) =
--     let
--       f' = (f { x = 1.0 }) :: { | headr (x :: Number | tailr) }
--     in
--       F f'

-- instance Convible F (headr (x :: Number | tailr)) where
--   conv
--     :: F (headr (x :: Number | tailr))
--     -> F (headr (x :: Number | tailr))
--   conv (F f) = F (f { x = 1.0 })

-- aa :: forall h t. Convible F (h (X t)) => Record (h (X t)) -> Record (h (X t))
-- aa = F >>> conv >>> \(F f) -> f

-- conv (F f) =
-- let
--   px = Proxy @"x"
-- in
--   F (set px 0.0 f)

{- conv𝒙 :: forall r. { x :: Number | r } -> { x :: Number | r }
conv𝒙 a = a { x = 1.0 }

conv𝒚 :: forall r. { y :: Number | r } -> { y :: Number | r }
conv𝒚 a = a { y = 1.0 }

conv𝒘 :: forall r. { w :: Number | r } -> { w :: Number | r }
conv𝒘 a = a { w = 1.0 }

conv𝒉 :: forall r. { h :: Number | r } -> { h :: Number | r }
conv𝒉 a = a { h = 1.0 } -}

{- ┌───────┐
-- │ Types │
-- └───────┘
--}

type Width r = (width :: Number | r)

type Height r = (height :: Number | r)

type Ratio r = (ratio :: AspectRatio | r)

type X r = (x :: Number | r)

type Y r = (y :: Number | r)

type Point = X & Y
type Point2 = X (Y ())
type Point3 = X <> Y $ ()

type Size = Width & Height

type Rect r = X + Y + Width + Height + r

{- toP :: forall r. Point r -> Point ()
toP { x, y } = { x, y } -}

type RecordOf :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> Type
type RecordOf a b = { | a + b + () }

infixl 0 type RecordOf as &

-- type RecordWith :: (Row Type -> Row Type) -> (Row Type -> Type) -> Type
-- type RecordWith a b = { | }

-- type HasRect r = X + Y + Width + Height ++ r
-- type HasRect r = { | X + Y + Width + Height + r }

-- type And2 :: forall k r. (Row k -> Row k) -> (Row k -> Row k) -> Row k
-- type And2 a b = forall (r :: Row k). a + b + r

type And0 :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> Row Type -> Row Type
type And0 a b r = a + b + r

type And :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> Type
type And a b = forall (r :: Row Type). { | a + b + r }

-- type AndK :: forall k. (Row k -> Row k) -> (Row k -> Row k) -> Row k
-- type AndK a b = forall (r :: Row k). a + b + r

type And2 :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> (Row Type -> Row Type)
type And2 a b r = a + b + r

infixr 9 type And2 as <>

type Has :: (Row Type -> Row Type) -> Type
type Has a = forall (r :: Row Type). { | a r }

type Only :: (Row Type -> Row Type) -> Type
type Only a = { | a () }

xx :: Point
xx = { x: 1.0, y: 1.0 }

yy :: Has Rect -> Only Rect -- { | Rect () }
yy { x, y, width, height } = { x, y, width, height }

yyy :: Has X -> Only X -- { | Rect () }
yyy { x } = { x }

yyyy :: Has (X <> Y) -> { | X (Y ()) } -- { | Rect () }
yyyy { x, y } = { x, y }

-- type XY = X (Y ())
-- type YX = Y (X ())

-- zz :: { | XY } -> { | YX }
-- zz a = a

-- type XYList :: RowList Type -> RowList Type
-- type XYList l = Cons "x" Number (Cons "y" Number l)

-- type XYRow :: Row Type -> Type
-- type XYRow r =
--   forall (xyr :: Row Type) (l :: RowList Type)
--    . RowToList r l
--   => ListToRow (XYList l) xyr
--   => { | xyr }

-- -- type YX_ = Cons "y" Number $ Cons "x" Number Nil

-- zzz :: { | XY } -> XYRow ()
-- zzz { x, y } = { x, y }
