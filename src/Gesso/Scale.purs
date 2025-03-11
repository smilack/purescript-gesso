module Gesso.Scale
  ( (@>)
  , (@>>)
  , (@@)
  , (@^)
  , (@^^)
  , Rect
  , Rectangular
  , Rectangular'
  , Scaler
  , ScalingFunctions
  , class Scalable
  , heightTo
  , mkScaler
  , scale
  , to
  , widthTo
  , xTo
  , yTo
  ) where

import Prelude

import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Tuple.Nested ((/\))
import Record (delete, get) as Record
import Record.Builder (Builder, buildFromScratch, nub)
import Record.Builder (insert) as Builder
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Lacks)
import Type.RowList (RowList, class RowToList, Cons, Nil)

-- ┌───────────────────────────┐
-- │ Dimension & scaling types │
-- └───────────────────────────┘

type Rectangular' :: Type -> Row Type
type Rectangular' a =
  ( x :: a
  , y :: a
  , width :: a
  , height :: a
  )

type Rectangular :: Type -> Type
type Rectangular a = { | Rectangular' a }

type Rect :: Type
type Rect = Rectangular Number

type ScalingFunctions :: Row Type
type ScalingFunctions = Rectangular' (Number -> Number)

type Scaler :: Type
type Scaler =
  { scaler ::
      forall rl r
       . RowToList r rl
      => Scalable rl r Number
      => { | r }
      -> Builder {} { | r }
  | ScalingFunctions
  }

-- ┌────────────────────┐
-- │ Scaling operations │
-- └────────────────────┘

to
  :: forall rl r
   . RowToList r rl
  => Scalable rl r Number
  => { | r }
  -> Scaler
  -> { | r }
to r { scaler } = buildFromScratch $ scaler r

xTo :: Number -> Scaler -> Number
xTo n { x } = x n

yTo :: Number -> Scaler -> Number
yTo n { y } = y n

widthTo :: Number -> Scaler -> Number
widthTo n { width } = width n

heightTo :: Number -> Scaler -> Number
heightTo n { height } = height n

infix 2 to as @@
infix 2 xTo as @>
infix 2 yTo as @^
infix 2 widthTo as @>>
infix 2 heightTo as @^^

-- ┌─────────────────┐
-- │ Scaler creation │
-- └─────────────────┘

mkScaler :: { | ScalingFunctions } -> Scaler
mkScaler fns@{ x, y, width, height } = { scaler, x, y, width, height }
  where
  scaler
    :: forall rl r
     . RowToList r rl
    => Scalable rl r Number
    => { | r }
    -> Builder {} { | r }
  scaler = scale @rl $ toMap fns

toMap :: { | ScalingFunctions } -> Map String (Number -> Number)
toMap { x, y, width, height } = Map.fromFoldable
  [ "x" /\ x
  , "y" /\ y
  , "width" /\ width
  , "height" /\ height
  ]

-- Alias for three constraints that appear together a lot
class CanInsert :: Symbol -> Type -> Row Type -> Row Type -> Constraint
class (IsSymbol k, Cons k a t r, Lacks k t) <= CanInsert k a t r

instance (IsSymbol k, Cons k a t r, Lacks k t) => CanInsert k a t r

-- | Walk through fields of record, if any value :: a, check if the map has a
-- | function with the same key, if so apply the function otherwise continue.
class Scalable :: RowList Type -> Row Type -> Type -> Constraint
class (RowToList r rl) <= Scalable rl r a | rl -> r where
  scale :: Map String (a -> a) -> { | r } -> Builder {} { | r }

instance scalableRecEmpty :: Scalable Nil () a where
  scale _ _ = nub

instance scalableRecScalableType ::
  ( RowToList r (Cons k a tl)
  , CanInsert k a t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k a tl) r a where
  scale m r = scaleRecScalableType @k m r <<< scaleRecRecur @tl @k m r

else instance scalableRecOtherType ::
  ( RowToList r (Cons k v tl)
  , CanInsert k v t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k v tl) r a where
  scale m r = scaleRecOtherType @k r <<< scaleRecRecur @tl @k m r

scaleRecScalableType
  :: forall @k a t r
   . CanInsert k a t r
  => Map String (a -> a)
  -> { | r }
  -> Builder { | t } { | r }
scaleRecScalableType m =
  insert @k <<< fromMaybe identity (lookup @k m) <<< get @k

scaleRecOtherType
  :: forall @k a t r
   . CanInsert k a t r
  => { | r }
  -> Builder { | t } { | r }
scaleRecOtherType = insert @k <<< get @k

scaleRecRecur
  :: forall @tl @k a v t r
   . RowToList t tl
  => Scalable tl t a
  => CanInsert k v t r
  => Map String (a -> a)
  -> { | r }
  -> Builder {} { | t }
scaleRecRecur m = scale @tl m <<< delete @k

-- Aliases for functions that use proxies
lookup :: forall @k a. IsSymbol k => Map String (a -> a) -> Maybe (a -> a)
lookup = Map.lookup $ reflectSymbol $ Proxy @k

insert :: forall @k a t r. CanInsert k a t r => a -> Builder { | t } { | r }
insert = Builder.insert $ Proxy @k

get :: forall @k a t r. CanInsert k a t r => { | r } -> a
get = Record.get $ Proxy @k

delete :: forall @k a t r. CanInsert k a t r => { | r } -> { | t }
delete = Record.delete $ Proxy @k