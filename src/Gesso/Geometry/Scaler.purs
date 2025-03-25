module Gesso.Geometry.Scaler
  ( (*~>)
  , (-~>)
  , (/~>)
  , (<~*)
  , (<~-)
  , (<~/)
  , (<~|)
  , (|~>)
  , Scaler
  , ScalingFunctions
  , class Scalable
  , from
  , lengthFrom
  , lengthTo
  , mkScaler
  , scale
  , to
  , xFrom
  , xTo
  , yFrom
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

-- | `x`, `y`, and `length` `Number -> Number` functions for converting between
-- | coordinate systems.
type ScalingFunctions :: Row Type
type ScalingFunctions =
  Position (Number -> Number) + (length :: Number -> Number)

-- | Data and functions for working with a coordinate system:
-- |
-- | The `x`, `y`, `width`, and `height` fields are the origin and size of the
-- | system. The `rect` field contains all of them as well - sometimes it's
-- | easier to access them indiviually, and sometimes it's easier to access them
-- | as a complete `Rect`.
-- |
-- | `scaling` contains functions to scale `x`, `y`, `length`, and entire
-- | records, but they're more convenient to use with the `to` and `from`
-- | functions rather than being called directly:
-- |
-- | ```purescript
-- | -- this:
-- | { x: 1.0, y: 2.0 } `to` canvas
-- |
-- | -- not:
-- | canvas.scaling.all { x: 1.0, y: 2.0 }
-- | ```
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

-- | Convert fields in an arbitrary record to the coordinate system of a
-- | `Scaler`. If any of these fields is found and has type `Number`, it will be
-- | converted:
-- |
-- | - `x` fields: `x`, `x1`, `x2`
-- | - `y` fields: `y`, `y1`, `y2`
-- | - `length` fields: `width`, `w`, `height`, `h`, `radius`, `r`, `length`,
-- |   `len`, `l`
-- |
-- | ```purescript
-- | line' = { x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0 } `to` canvas
-- | circle' = { x: 0.0, y: 0.0, r: 1.0 } `to` canvas
-- | ```
to
  :: forall rl r
   . RowToList r rl
  => Scalable rl r Number
  => { | r }
  -> Scaler
  -> { | r }
to r { scaling: { all } } = buildFromScratch $ all r

-- | [`to`](#v:to) with arguments flipped:
-- |
-- | ```purescript
-- | line' = canvas `from` { x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0 }
-- | circle' = canvas `from` { x: 0.0, y: 0.0, r: 1.0 }
-- | ```
from
  :: forall rl r
   . RowToList r rl
  => Scalable rl r Number
  => Scaler
  -> { | r }
  -> { | r }
from = flip to

-- | Convert a single `x` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | x' = x `xTo` canvas
-- | ```
xTo :: Number -> Scaler -> Number
xTo n { scaling: { x } } = x n

-- | [`xTo`](#v:xTo) with arguments flipped:
-- | ```purescript
-- | x' = canvas `xFrom` x
-- | ```
xFrom :: Scaler -> Number -> Number
xFrom = flip xTo

-- | Convert a single `y` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | y' = y `yTo` canvas
-- | ```
yTo :: Number -> Scaler -> Number
yTo n { scaling: { y } } = y n

-- | [`yTo`](#v:yTo) with arguments flipped:
-- | ```purescript
-- | y' = canvas `yFrom` y
-- | ```
yFrom :: Scaler -> Number -> Number
yFrom = flip yTo

-- | Convert a single `length` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | l' = l `lengthTo` canvas
-- | ```
lengthTo :: Number -> Scaler -> Number
lengthTo n { scaling: { length } } = length n

-- | [`lengthTo`](#v:lengthTo) with arguments flipped:
-- | ```purescript
-- | l' = canvas `lengthFrom` l
-- | ```
lengthFrom :: Scaler -> Number -> Number
lengthFrom = flip lengthTo

-- | Convert an arbitrary record to the coordinate system of a `Scaler`:
-- | ```purescript
-- | line' = { x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0 } *~> canvas
-- | circle' = { x: 0.0, y: 0.0, r: 1.0 } *~> canvas
-- | ```
infix 2 to as *~>

-- | [`to`](#v:to) with arguments flipped:
-- | ```purescript
-- | line' = canvas <~* { x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0 }
-- | circle' = canvas <~* { x: 0.0, y: 0.0, r: 1.0 }
-- | ```
infix 2 from as <~*

-- | Convert a single `x` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | x' = x -~> canvas
-- | ```
infix 2 xTo as -~>

-- | [`xTo`](#v:xTo) with arguments flipped:
-- | ```purescript
-- | x' = canvas <~- x
-- | ```
infix 2 xFrom as <~-

-- | Convert a single `y` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | y' = y |~> canvas
-- | ```
infix 2 yTo as |~>

-- | [`yTo`](#v:yTo) with arguments flipped:
-- | ```purescript
-- | y' = canvas <~| y
-- | ```
infix 2 yFrom as <~|

-- | Convert a single `length` value to the coordinate system of a `Scaler`:
-- | ```purescript
-- | l' = l /~> canvas
-- | ```
infix 2 lengthTo as /~>

-- | [`lengthTo`](#v:lengthTo) with arguments flipped:
-- | ```purescript
-- | l' = canvas <~/ l
-- | ```
infix 2 lengthFrom as <~/

-- ┌─────────────────┐
-- │ Scaler creation │
-- └─────────────────┘

-- | Create a `Scaler` record for a coordinate system using its dimensions and
-- | `x`, `y`, and `length` scaling functions.
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

-- | Typeclass for an arbitrary record. `scale` walks through the fields of a
-- | record, and if any value `:: a`, checks if the map has a function with the
-- | same key. If so, applies the function to the value.
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
