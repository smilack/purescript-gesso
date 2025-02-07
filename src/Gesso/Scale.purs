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

type X r = (x :: Number | r)
type Y r = (y :: Number | r)
type Width r = (width :: Number | r)
type Height r = (height :: Number | r)

type Scalable r = X + Y + Width + Height + r

-- | Alias for record builder that applies a function to one or more fields in a
-- | record without changing their types.
type ScalerFor :: (Row Type -> Row Type) -> Type
type ScalerFor f = forall r. Builder { | f r } { | f r }

type ScalingFunctions =
  { x :: Number -> Number
  , y :: Number -> Number
  , width :: Number -> Number
  , height :: Number -> Number
  }

-- | A record containing scalers for all scalable fields.
type Scalers r =
  { x :: ScalerFor X
  , y :: ScalerFor Y
  , width :: ScalerFor Width
  , height :: ScalerFor Height
  , all :: Builder { | Scalable r } { | Scalable r }
  }

-- ┌────────────────────┐
-- │ Scaling operations │
-- └────────────────────┘

defaults :: Record (Scalable ())
defaults = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

fill
  :: forall partial union complete
   . Union partial (Scalable ()) union
  => Nub union complete
  => Builder (Record partial) (Record complete)
fill = flip merge defaults

-- extract
--   :: forall keys @required from all diff filler
--    . RowToList required keys
--   => RowToList all from
--   => Del keys from diff
--   => Union required filler all
--   => Keys keys
--   => { | required }
--   -> { | all }
--   -> { | required }
scale
  :: forall r l
   . RowToList r l
  => Scalers r
  -> Record r
  -> Record r
scale { all } r =
  extract @r @l r
    $ build (all <<< fill) r

mkScaler
  :: forall @sym a r row
   . IsSymbol sym
  => Cons sym a r row
  => (a -> a)
  -> Builder (Record row) (Record row)
mkScaler = modify (Proxy @sym)

mkScalers :: forall r. ScalingFunctions -> Scalers r
mkScalers fs = { x, y, width, height, all }
  where
  x :: ScalerFor X
  x = mkScaler @"x" fs.x

  y :: ScalerFor Y
  y = mkScaler @"y" fs.y

  width :: ScalerFor Width
  width = mkScaler @"width" fs.width

  height :: ScalerFor Height
  height = mkScaler @"height" fs.height

  all :: ScalerFor Scalable
  all = x <<< y <<< width <<< height

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
extract
  :: forall @keys @required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Del keys from diff
  => Union required filler all
  => Keys keys
  => { | required }
  -> { | all }
  -> { | required }
extract _ = pick

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
