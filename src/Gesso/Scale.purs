module Gesso.Scale where

import Prelude

import Record.Builder (modify, build, Builder, flip, merge)
import Record.Extra (class Keys, pick)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, type (+), RowApply, class Cons, class Union, class Nub)
import Type.RowList (RowList, Nil, Cons, class RowToList, class ListToRow, class RowListAppend, class RowListNub)

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
type Dimensioned a r = Record (Dimensioned' a r)

type Rect' :: Row Type
type Rect' = Dimensioned' Number ()

type Rect :: Type
type Rect = Record Rect'

type ScalingFunctions :: Type
type ScalingFunctions = Record (Dimensioned' (Number -> Number) ())

type Scaler :: Type
type Scaler = forall r. Builder (Dimensioned Number r) (Dimensioned Number r)

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

-- | Find values of all keys in `all` that also appear in `required`.
extract
  :: forall keys required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Del keys from diff
  => Union required filler all
  => Keys keys
  => { | required }
  -> { | all }
  -> { | required }
extract _ = pick

scale
  :: forall partial union complete keys from diff filler
   . Union partial Rect' union
  => Nub union complete
  => RowToList partial keys
  => RowToList complete from
  => Del keys from diff
  => Union partial filler complete
  => Keys keys
  => Builder (Record complete) (Record complete)
  -> Record partial
  -> Record partial
scale scaler r = extract r $ build (scaler <<< fill) r

mkScaler
  :: forall @l a t r
   . IsSymbol l
  => Cons l a t r
  => (a -> a)
  -> Builder (Record r) (Record r)
mkScaler = modify (Proxy @l)

mkScalers :: ScalingFunctions -> Scaler
mkScalers fs =
  mkScaler @"x" fs.x
    <<< mkScaler @"y" fs.y
    <<< mkScaler @"width" fs.width
    <<< mkScaler @"height" fs.height

-- all :: ScalerFor (All Number)
-- all = x <<< y <<< width <<< height

-- ┌──────────────────────────┐
-- │ Row Difference typeclass │
-- └──────────────────────────┘

-- | Find keys in `from` that don't exist in `keys`. `from` is assumed to be a
-- | superset of `keys`, which simplifies a couple things:
-- |
-- |  - There's no need for an instance where `from` is `Nil` but `keys` is not.
-- |
-- |  - When the heads of `keys` and `from` are different, we only need to
-- |    advance `from`, because the key will be in there later.
class Del :: RowList Type -> RowList Type -> RowList Type -> Constraint
class Del keys from diff | keys from -> diff

instance delEmpty :: Del Nil Nil Nil

else instance delNoMoreKeys :: Del Nil from from

else instance delDeleteKey ::
  ( Del keyTail fromTail diff
  ) =>
  Del (Cons key a keyTail) (Cons key a fromTail) diff

else instance delKeepKey ::
  ( Del (Cons key a keyTail) fromTail diffTail
  ) =>
  Del (Cons key a keyTail) (Cons key' b fromTail) (Cons key' b diffTail)

-- | Find values of all keys in `all` that do not appear in `required`.
delete
  :: forall keys required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Del keys from diff
  => RowToList filler diff
  => Union filler required all
  => Keys diff
  => { | required }
  -> { | all }
  -> { | filler }
delete _ = pick

-- | Find values of all keys in `all` that also appear in `required`.
-- extract
--   :: forall @keys @required from all diff filler
--    . RowToList required keys
--   => RowToList all from
--   => Del keys from diff
--   => Union required filler all
--   => Keys keys
--   => { | required }
--   -> { | all }
--   -> { | required }
-- extract _ = pick

-- ┌─────────────────────────┐
-- │ Row inclusion typeclass │
-- └─────────────────────────┘

class Has :: Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class Has a b u n | a b -> u n

instance
  ( RowToList a la
  , RowToList b lb
  , Del lb la lc
  , Union a b u
  , Nub u n
  ) =>
  Has a b u n

-- ┌───────────────┐
-- │ Utility types │
-- └───────────────┘

-- | Alias just so I don't have to break long lines
type OpenRow = Row Type -> Row Type

-- | Combine two open rows and close them
type RowApplyClose :: OpenRow -> OpenRow -> Row Type
type RowApplyClose a b = a + b + ()

infixr 0 type RowApplyClose as &

-- | Combine two open rows and leave them open
type RowApplyOpen :: OpenRow -> OpenRow -> OpenRow
type RowApplyOpen a b r = a + b + r

infixr 1 type RowApplyOpen as ++

{-
x:      -@   @>
width:  -@-  @>>
y:      |@   @^
height: |@|  @^^
-}

-- instance
--   -- ( Cons "x" Number r r1
--   -- -- , RecordToRow rec r1
--   -- ) =>
--   ScalableField (Field "x" Number tail row) { | row } where
--   scale
--     :: Cons "x" Number r r1
--     => { | r1 }
--     -> { | r1 }
--   scale rec =
--     let
--       r :: ?e
--       r = rec
--     in
--       -- (modify (Proxy @"x") identity rec) --(r { x = r.x })
--       rec

-- Field rec
