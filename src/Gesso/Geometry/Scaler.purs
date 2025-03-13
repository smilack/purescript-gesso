module Gesso.Geometry.Scaler
  ( (>@)
  , (@<~)
  , (^@)
  , (~>@)
  , (-@)
  , Scaler
  , ScalingFunctions
  , class Scalable
  , from
  , lengthTo
  , mkScaler
  , scale
  , to
  , xTo
  , yTo
  ) where

import Prelude

import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Tuple.Nested ((/\))
import Gesso.Geometry.Dimensions (Position, Rect, Rectangular)
import Record (delete, get) as Record
import Record.Builder (Builder, buildFromScratch, nub)
import Record.Builder (insert) as Builder
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Lacks, type (+))
import Type.RowList (RowList, class RowToList, Cons, Nil)

-- ┌───────────────────────────┐
-- │ Dimension & scaling types │
-- └───────────────────────────┘
type ScalingFunctions :: Row Type
type ScalingFunctions =
  Position (Number -> Number) + (length :: Number -> Number)

type Scaler :: Type
type Scaler =
  { | Rectangular Number +
      ( scaling ::
          { all ::
              forall rl r
               . RowToList r rl
              => Scalable rl r Number
              => { | r }
              -> Builder {} { | r }
          | ScalingFunctions
          }
      , rect :: Rect
      )
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
to r { scaling: { all } } = buildFromScratch $ all r

from
  :: forall rl r
   . RowToList r rl
  => Scalable rl r Number
  => Scaler
  -> { | r }
  -> { | r }
from = flip to

xTo :: Number -> Scaler -> Number
xTo n { scaling: { x } } = x n

yTo :: Number -> Scaler -> Number
yTo n { scaling: { y } } = y n

lengthTo :: Number -> Scaler -> Number
lengthTo n { scaling: { length } } = length n

infix 2 to as ~>@
infix 2 from as @<~
infix 2 xTo as >@
infix 2 yTo as ^@
infix 2 lengthTo as -@

-- ┌─────────────────┐
-- │ Scaler creation │
-- └─────────────────┘

mkScaler :: Rect -> { | ScalingFunctions } -> Scaler
mkScaler rect fns =
  { x: rect.x
  , y: rect.y
  , width: rect.width
  , height: rect.height
  , rect
  , scaling:
      { x: fns.x
      , y: fns.y
      , length: fns.length
      , all
      }
  }
  where
  all
    :: forall rl r
     . RowToList r rl
    => Scalable rl r Number
    => { | r }
    -> Builder {} { | r }
  all = scale @rl $ toMap fns

toMap :: { | ScalingFunctions } -> Map String (Number -> Number)
toMap { x, y, length } = Map.fromFoldable
  [ "x" /\ x
  , "x1" /\ x
  , "x2" /\ x
  , "y" /\ y
  , "y1" /\ y
  , "y2" /\ y
  , "width" /\ length
  , "w" /\ length
  , "height" /\ length
  , "h" /\ length
  , "radius" /\ length
  , "r" /\ length
  , "length" /\ length
  , "len" /\ length
  , "l" /\ length
  ]

-- | Walk through fields of record, if any value :: a, check if the map has a
-- | function with the same key, if so apply the function otherwise continue.
class Scalable :: RowList Type -> Row Type -> Type -> Constraint
class (RowToList r rl) <= Scalable rl r a | rl -> r where
  scale :: Map String (a -> a) -> { | r } -> Builder {} { | r }

instance scalableRecEmpty :: Scalable Nil () a where
  scale _ _ = nub

instance scalableRecScalableType ::
  ( RowToList r (Cons k a tl)
  , IsSymbol k
  , Cons k a t r
  , Lacks k t
  , Scalable tl t a
  ) =>
  Scalable (Cons k a tl) r a where
  scale m r = scaleRecScalableType @k m r <<< scaleRecRecur @tl @k m r

else instance scalableRecOtherType ::
  ( RowToList r (Cons k v tl)
  , IsSymbol k
  , Cons k v t r
  , Lacks k t
  , Scalable tl t a
  ) =>
  Scalable (Cons k v tl) r a where
  scale m r = scaleRecOtherType @k r <<< scaleRecRecur @tl @k m r

scaleRecScalableType
  :: forall @k a t r
   . IsSymbol k
  => Cons k a t r
  => Lacks k t
  => Map String (a -> a)
  -> { | r }
  -> Builder { | t } { | r }
scaleRecScalableType m =
  insert @k <<< fromMaybe identity (lookup @k m) <<< get @k

scaleRecOtherType
  :: forall @k a t r
   . IsSymbol k
  => Cons k a t r
  => Lacks k t
  => { | r }
  -> Builder { | t } { | r }
scaleRecOtherType = insert @k <<< get @k

scaleRecRecur
  :: forall @tl @k a v t r
   . RowToList t tl
  => Scalable tl t a
  => IsSymbol k
  => Cons k v t r
  => Lacks k t
  => Map String (a -> a)
  -> { | r }
  -> Builder {} { | t }
scaleRecRecur m = scale @tl m <<< delete @k

-- Aliases for functions that use proxies
lookup :: forall @k a. IsSymbol k => Map String (a -> a) -> Maybe (a -> a)
lookup = Map.lookup $ reflectSymbol $ Proxy @k

insert
  :: forall @k a t r
   . IsSymbol k
  => Cons k a t r
  => Lacks k t
  => a
  -> Builder { | t } { | r }
insert = Builder.insert $ Proxy @k

get :: forall @k a t r. IsSymbol k => Cons k a t r => Lacks k t => { | r } -> a
get = Record.get $ Proxy @k

delete
  :: forall @k a t r
   . IsSymbol k
  => Cons k a t r
  => Lacks k t
  => { | r }
  -> { | t }
delete = Record.delete $ Proxy @k
