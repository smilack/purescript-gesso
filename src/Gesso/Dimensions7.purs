module Gesso.Dimensions7 where

import Prelude

import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Record (get, delete)
import Record.Builder (Builder, insert, buildFromScratch)
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
tst1 = scale conversions { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 }

tst2 :: { x1 :: Number }
tst2 = scale conversions { x1: 1.0 }

tst3 :: { z :: Number }
tst3 = scale conversions { z: 1.0 }

tst4 :: { x2 :: Number, y2 :: Number, z2 :: Number }
tst4 = scale conversions { x2: 1.0, y2: 1.0, z2: 1.0 }

fs :: { x :: Number -> Number, y :: Number -> Number }
fs =
  { x: (_ * 2.0)
  , y: (_ * 10.0)
  }

type Fields :: Type -> Row Type
type Fields a = (x1 :: a, y1 :: a, x2 :: a, y2 :: a)

conversions :: Map String (Number -> Number)
conversions = fromFoldable
  [ "x1" /\ fs.x
  , "x2" /\ fs.x
  , "y1" /\ fs.y
  , "y2" /\ fs.y
  ]

getF :: forall a @sym. IsSymbol sym => Map String (a -> a) -> Maybe (a -> a)
getF = lookup $ reflectSymbol $ Proxy @sym

class Scalable :: RowList Type -> Row Type -> Type -> Constraint
class (RowToList r rl) <= Scalable rl r a | rl -> r a where
  scaler :: Map String (a -> a) -> { | r } -> Builder {} { | r }

instance
  ( RowToList r (Cons k a Nil)
  , IsSymbol k
  , Cons k a () r
  ) =>
  Scalable (Cons k a Nil) r a where
  scaler :: Map String (a -> a) -> { | r } -> Builder {} { | r }
  scaler m r =
    let
      f = getF @k m
      v = get (Proxy @k) r
      v' = fromMaybe identity f $ v
    in
      insert (Proxy @k) v'

else instance
  ( RowToList r (Cons k a tl)
  , IsSymbol k
  , Cons k a t r
  , Lacks k t
  , Scalable tl t a
  ) =>
  Scalable (Cons k a tl) r a where
  scaler
    :: Map String (a -> a)
    -> { | r }
    -> Builder {} { | r }
  scaler m r =
    let
      t :: { | t }
      t = delete (Proxy @k) r

      tbuilder :: Builder {} { | t }
      tbuilder = scaler @tl m t

      f :: Maybe (a -> a)
      f = getF @k m

      v :: a
      v = get (Proxy @k) r

      v' :: a
      v' = fromMaybe identity f $ v

      rbuilder :: Builder { | t } { | r }
      rbuilder = insert (Proxy @k) v'
    in
      rbuilder <<< tbuilder

else instance
  ( RowToList r (Cons k v tl)
  , IsSymbol k
  , Cons k v t r
  , Lacks k t
  , Scalable tl t a
  ) =>
  Scalable (Cons k v tl) r a where
  scaler
    :: Map String (a -> a)
    -> { | r }
    -> Builder {} { | r }
  scaler m r =
    let
      t :: { | t }
      t = delete (Proxy @k) r

      tbuilder :: Builder {} { | t }
      tbuilder = scaler @tl m t

      v :: v
      v = get (Proxy @k) r

      rbuilder :: Builder { | t } { | r }
      rbuilder = insert (Proxy @k) v
    in
      rbuilder <<< tbuilder
