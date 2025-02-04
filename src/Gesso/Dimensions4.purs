module Gesso.Dimensions4 where

import Prelude

import Type.Proxy (Proxy(..))
import Type.Row (class Union, class Nub)
import Type.RowList (class RowToList, RowList, Nil, Cons)

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

type All = (height :: Number, width :: Number, x :: Number, y :: Number)
type X = (x :: Number)
type Y = (y :: Number)
type Width = (width :: Number)
type Height = (height :: Number)
type Point = (x :: Number, y :: Number)
type Size = (height :: Number, width :: Number)

has :: forall a b u n. Has a b u n => Proxy a -> Proxy b -> Unit
has _ _ = unit

-- Good
good :: Array Unit
good =
  -- All has everything
  [ has (Proxy @All) (Proxy @X)
  , has (Proxy @All) (Proxy @Y)
  , has (Proxy @All) (Proxy @Width)
  , has (Proxy @All) (Proxy @Height)
  , has (Proxy @All) (Proxy @Point)
  , has (Proxy @All) (Proxy @Size)
  -- Point has X and Y
  , has (Proxy @Point) (Proxy @X)
  , has (Proxy @Point) (Proxy @Y)
  -- Size has Width and Height
  , has (Proxy @Size) (Proxy @Height)
  , has (Proxy @Size) (Proxy @Width)
  -- Everything has itself
  , has (Proxy @X) (Proxy @X)
  , has (Proxy @Y) (Proxy @Y)
  , has (Proxy @Width) (Proxy @Width)
  , has (Proxy @Height) (Proxy @Height)
  , has (Proxy @Point) (Proxy @Point)
  , has (Proxy @Size) (Proxy @Size)
  , has (Proxy @All) (Proxy @All)
  ]

bad :: Array Unit
bad =
  [
  -- Nothing has All
  -- has (Proxy @X) (Proxy @All)
  -- has (Proxy @Y) (Proxy @All)
  -- has (Proxy @Width) (Proxy @All)
  -- has (Proxy @Height) (Proxy @All)
  -- has (Proxy @Point) (Proxy @All)
  -- has (Proxy @Size) (Proxy @All)
  -- Spot checks
  -- has (Proxy @Point) (Proxy @Size)
  -- has (Proxy @X) (Proxy @Y)
  ]

-- infix 0 type Has as ⊂

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
