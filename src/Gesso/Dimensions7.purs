module Gesso.Dimensions7 where

import Prelude

import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Record (get, delete) as Record
import Record.Builder (Builder, buildFromScratch)
import Record.Builder (insert) as Builder
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Lacks)
import Type.RowList (RowList, class RowToList, Cons, Nil)

main :: Effect Unit
main = do
  logShow tst1
  logShow tst2
  logShow tst3
  logShow tst4

scale :: forall rl r a. RowToList r rl => Scalable rl r a => Map String (a -> a) -> { | r } -> { | r }
scale m r = buildFromScratch (scaler @rl m r)

tst1 :: { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
tst1 = scale (toConversions fs) { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 }

tst2 :: { x1 :: Number }
tst2 = scale (toConversions fs) { x1: 1.0 }

tst3 :: { z :: Number }
tst3 = scale (toConversions fs) { z: 1.0 }

tst4 :: { x2 :: Number, y2 :: Number, z2 :: Number }
tst4 = scale (toConversions fs) { x2: 1.0, y2: 1.0, z2: 1.0 }

type BasicConversions =
  { x :: Number -> Number
  , y :: Number -> Number
  , width :: Number -> Number
  , height :: Number -> Number
  }

fs :: BasicConversions
fs =
  { x: (_ * 2.0)
  , y: (_ * 10.0)
  , width: identity
  , height: identity
  }

toConversions :: BasicConversions -> Map String (Number -> Number)
toConversions { x, y } = Map.fromFoldable
  [ "x1" /\ x
  , "x2" /\ x
  , "y1" /\ y
  , "y2" /\ y
  ]

--
-- Scaler helper class
--

class Buildable :: Symbol -> Type -> Row Type -> Row Type -> Constraint
class (IsSymbol k, Cons k a t r, Lacks k t) <= Buildable k a t r

instance (IsSymbol k, Cons k a t r, Lacks k t) => Buildable k a t r

--
-- Scaler instances
--

class Scalable :: RowList Type -> Row Type -> Type -> Constraint
class (RowToList r rl) <= Scalable rl r a | rl -> r where
  scaler :: Map String (a -> a) -> { | r } -> Builder {} { | r }

instance
  ( RowToList r (Cons k a Nil)
  , Buildable k a () r
  ) =>
  Scalable (Cons k a Nil) r a where
  scaler = scalerSameType @k

else instance
  ( RowToList r (Cons k v Nil)
  , Buildable k v () r
  ) =>
  Scalable (Cons k v Nil) r a where
  scaler _ = scalerDiffType @k

else instance
  ( RowToList r (Cons k a tl)
  , Buildable k a t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k a tl) r a where
  scaler m r = scalerSameType @k m r <<< scaler @tl m (delete @k r)

else instance
  ( RowToList r (Cons k v tl)
  , Buildable k v t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k v tl) r a where
  scaler m r = scalerDiffType @k r <<< scaler @tl m (delete @k r)

--
-- Scaler impl
--

scalerSameType
  :: forall @k a t r
   . Buildable k a t r
  => Map String (a -> a)
  -> { | r }
  -> Builder { | t } { | r }
scalerSameType m = insert @k <<< fromMaybe identity (lookup @k m) <<< get @k

scalerDiffType
  :: forall @k a t r
   . Buildable k a t r
  => { | r }
  -> Builder { | t } { | r }
scalerDiffType = insert @k <<< get @k

--
-- Aliases for functions that need proxies
--

lookup :: forall @k a. IsSymbol k => Map String (a -> a) -> Maybe (a -> a)
lookup = Map.lookup $ reflectSymbol $ Proxy @k

insert :: forall @k a t r. Buildable k a t r => a -> Builder { | t } { | r }
insert = Builder.insert $ Proxy @k

get :: forall @k a t r. Buildable k a t r => { | r } -> a
get = Record.get $ Proxy @k

delete :: forall @k a t r. Buildable k a t r => { | r } -> { | t }
delete = Record.delete $ Proxy @k