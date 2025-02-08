module Gesso.Dimensions5 where

import Prelude hiding (flip)

import Prim.Boolean (True)
import Record (get)
import Record.Builder (Builder, merge, flip, modify, build, nub)
import Type.Data.Symbol (class Equals)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Union, class Cons, class Nub, class Lacks)
import Type.RowList (RowList, Cons, Nil, class RowToList, class ListToRow)

type XRow = (x :: Number, x1 :: Number, x2 :: Number)
type YRow = (y :: Number, y1 :: Number, y2 :: Number)
type WidthRow = (width :: Number, w :: Number, w1 :: Number, w2 :: Number, width1 :: Number, width2 :: Number)
type HeightRow = (height :: Number, h :: Number, h1 :: Number, h2 :: Number, height1 :: Number, height2 :: Number)

type TestRow = (x :: Number, y :: Number)

-- type Fields :: forall k. k -> Row k
-- type Fields a = (x :: a, y :: a, w :: a, h :: a)

-- type Rect = Fields Number
-- type Converters = Fields (Number -> Number)

-- defaults :: { | Fields Number }
-- defaults = { x: 0.0, y: 0.0, w: 0.0, h: 0.0 }

-- addDefaults
--   :: forall r u r'
--    . Union r Rect u
--   => Nub u r'
--   => Builder { | defaults } { | r' }
-- addDefaults = flip merge defaults

type Fields a = (x :: a, y :: a)

-- | The reason this fails when I try to add the `modify`s is that the fields
-- | in `r` take precedence over the fields in `Fields a`, and the `x`/`y`
-- | fields in `r` are *not* guaranteed to be `:: a`.
-- |
-- | *I think*
-- |
-- | Also?? It doesn't seem to propagate equality of symbols?
-- |
-- scale
--   :: forall a r u combined
--    . Union r (Fields a) u
--   => Nub u combined
--   => Record (Fields (a -> a))
--   -> Record (Fields a)
--   -> Record r
--   -> Record combined
-- scale fs defaults r = build (merge r) defaults

-- scale
--   :: forall a r u combined rtx rty
--    . Cons "x" a rtx r
--   => Cons "y" a rty r
--   => Union r (Fields a) u
--   => Nub u combined
--   => Record (Fields (a -> a))
--   -> Record (Fields a)
--   -> Record r
--   -> Record combined
-- scale fs defaults r = build (merge r) defaults

{- scale
  :: forall a r union complete sx tx tx' sy ty ty'
   
  -- merge input and defaults
  . Union r (x :: a, y :: a) union
  => Nub union complete
  -- x symbol
  => Equals "x" sx True
  => IsSymbol sx
  => Cons sx a tx r
  => Cons sx a tx' complete
  -- y symbol
  => Equals "y" sy True
  => IsSymbol sy
  => Cons sy a ty r
  => Cons sy a ty' complete
  -- params
  => Record (x :: a -> a, y :: a -> a)
  -> Record (x :: a, y :: a)
  -> Record r
  -> Record complete
scale fs defaults rIn = build (scaleY <<< scaleX <<< combine) defaults
  where
  combine :: Builder { x :: a, y :: a } { | complete }
  combine = merge rIn

  scaleX :: Builder { | complete } { | complete }
  scaleX = modify (Proxy @sx) fs.x

  scaleY :: Builder { | complete } { | complete }
  scaleY = modify (Proxy @sy) fs.y -}

-- scale
--   :: forall a r union complete sx tx tx'

--   -- merge input and defaults
--   . Has r (x :: a, y :: a)
--   => Union r (x :: a, y :: a) union
--   => Equals sx "x" True
--   => IsSymbol sx
--   => Cons sx a tx r
--   => Cons sx a tx' complete
--   => Nub union complete
--   => Record (x :: a -> a, y :: a -> a)
--   -> Record (x :: a, y :: a)
--   -> Record r
--   -> Record complete
-- scale fs defaults rIn = build ( {- scaleY <<< scaleX <<< -} combine) defaults
--   where
--   combine :: Builder { x :: a, y :: a } { | complete }
--   combine = merge rIn

--   scaleX :: Builder { | complete } { | complete }
--   scaleX = modify (Proxy @sx) fs.x

-- change
--   :: forall a r union complete
--    . Union r (x :: a) union
--   => Nub union complete
--   => Record (x :: a -> a)
--   -> Record (x :: a)
--   -> Record r
--   -> Record r
-- change fs default r = build (modify (Proxy @"x") fs.x) r

-- change'
--   :: forall a r union complete rl dl cl
--    . Union r (Fields a) union
--   => Nub union complete
--   => RowToList (Fields a) dl
--   => RowToList r rl
--   => RowToList complete cl
--   => HasTypeOrLacks rl dl
--   -- => HasTypeOrLacks cl dl
--   => Record (Fields (a -> a))
--   -> Proxy (Fields a)
--   -> Proxy r
--   -> Proxy complete
-- change' _ _ _ = Proxy

-- change' fs default r = build (merge r) default

-- a :: ?e
-- a = change' { x: const 0, y: const 0 } (Proxy :: Proxy (Fields Int)) (Proxy :: Proxy ())

class Scalable :: Row Type -> Constraint
class Scalable row

instance
  ( RowToList row rl
  , Cons "x" Number tx row
  , Cons "y" Number ty row
  ) =>
  Scalable row

htol
  :: forall r rl dl
   . RowToList (Fields Int) dl
  => RowToList r rl
  => HasTypeOrLacks dl rl r
  => Proxy r
  -> Proxy r
htol _ = Proxy

-- a0 :: Proxy ()
-- a0 = htol (Proxy :: Proxy ())

-- a1 :: Proxy (x :: Int)
-- a1 = htol (Proxy :: Proxy (x :: Int))

-- a2 :: Proxy (y :: Int)
-- a2 = htol (Proxy :: Proxy (y :: Int))

-- a3 :: Proxy (x :: Int, y :: Int)
-- a3 = htol (Proxy :: Proxy (x :: Int, y :: Int))

-- a4 = htol $ Proxy @(x :: Int, y :: Int, z :: Int)

class HasTypes :: RowList Type -> RowList Type -> Constraint
class HasTypes rl dict

-- instance HasTypes Nil dict

-- else instance
--   ( RowToList dr dict
--   , Cons k a dr' dr
--   , HasTypeOrLacks rlt dict
--   ) =>
--   HasTypes (Cons k a rlt) dict

class HasTypeOrLacks :: RowList Type -> RowList Type -> Row Type -> Constraint
class HasTypeOrLacks dict rl row | rl -> row

instance HasTypeOrLacks Nil rl row

-- else instance
--   ( Cons k2 a2 dict' dict
--   , HasTypeOrLacks 
--   )

else instance
  ( RowToList row rl
  , Cons k a row' row
  , HasTypeOrLacks dict' rl row
  ) =>
  HasTypeOrLacks (Cons k a dict') rl row

else instance
  ( RowToList row rl
  , Lacks k row
  , HasTypeOrLacks dict' rl row
  ) =>
  HasTypeOrLacks (Cons k a dict') rl row

-- both empty ? ok
-- no more
-- 
-- n
-- for each key in dict
--    if key matches or no match

-- where
-- convert :: Builder { x :: a, y :: a } { | r' }
-- convert = modify (Proxy @"x") x
--   <<< modify (Proxy @"y") y
--   <<< merge r

-- class Has row field

-- instance (Cons field Number tail row) => Has row field

-- merge defaults
--   :: Union defaults r _
--   => Nub _ out
--   => Record defaults
--   -> Builder (Record r) (Record out)

-- flip merge defaults
--   :: Union r defaults _
--   => Nub _ out
--   => Record r
--   -> Builder (Record defaults) (Record out)

-- flip
--   :: (Record a -> Builder (Record b) (Record out))
--   -> Record b
--   -> Builder (Record a) (Record out)

-- class Scale :: RowList Type -> Row Type -> Row Type -> Constraint
-- class Scale keysl fs row | keysl -> fs row where
--   builder :: { | fs } -> Builder { | row } { | row }

-- instance Scale Nil fs () where
--   builder _ = nub

-- instance (IsSymbol k, Cons k Number tr row, Cons k (Number -> Number) tf fs) => Scale (Cons k Number tl) fs row where
--   builder = modify (Proxy @k) <<< get (Proxy @k)

-- else instance (Nub row row) => Scale (Cons k Number tl) fs row where
--   builder _ = nub

-- to
--   :: forall keysl row
--    . RowToList Converters keysl
--   => Scale keysl Converters row
--   => { | row }
--   -> { | Converters }
--   -> { | row }
-- to r { x, y, w, h } = r

-- class ScaleAll :: RowList Type -> Constraint
-- class ScaleAll rl where
--   scaleAll :: forall row. RowToList row rl => { | row } -> { | Converters } -> { | row }

-- instance ScaleAll Nil where
--   scaleAll r _ = r

-- instance (ScaleAll tr) => ScaleAll (Cons k Number tr) where
--   scaleAll :: forall row. RowToList row rl => { | row } -> { | Converters } -> { | row }
--   scaleAll r fs = 

-- ┌─────────────────────────┐
-- │ Row inclusion typeclass │
-- └─────────────────────────┘
class Has :: Row Type -> Row Type -> {- Row Type -> Row Type ->  -} Constraint
class Has r tys

instance
  ( RowToList r rl
  , RowToList tys tysl
  , Del tysl rl l0
  ) =>
  Has r tys

{- type All = (height :: Number, width :: Number, x :: Number, y :: Number)
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
  ] -}

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