-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions3 where

import Prelude

import Record (get, set, modify, insert, delete) as R
import Record.Builder (modify, insert, delete, build, buildFromScratch, Builder, flip, merge)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, type (+), RowApply, class Cons, class Union, class Nub)
import Type.RowList (RowList, Nil, Cons, class RowToList, class ListToRow)

{- Current idea:
  (inspired by https://github.com/natefaubion/purescript-convertable-options/blob/main/README.md)
   
   Convenience:
    generic functions to create scaler function for a field (mkConverter)

   Meat:
     Make function that converts a record with all fields
     Wrapper functions to add and remove defaults for unsupplied fields.

   E.g.:
     1. scale { x: ..., y: ... }
     2. adds default `w` and `h`
     3. scales all fields
     4. removes `w` and `h`
     5. returns scaled `x` and `y`
  
    Progress:
      mkConverter is good
      need to do the merging and picking

    Also need to think about how the toDrawing/toPage indications will work. Explicit? Implicit?
-}

-- ┌──────────┐
-- │ Defaults │
-- └──────────┘

type ConvertibleFields = X ++ Y ++ Width ++ Height + ()

defaults :: { | ConvertibleFields }
defaults = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

addDefaults
  :: forall given all nub
   . Union given ConvertibleFields all
  => Nub all nub
  => Builder { | given } { | nub }
addDefaults = flip merge defaults

-- removeDefaults
--   :: forall given all nub keys from into gotten
--    . Union given ConvertibleFields all
--   => Nub all nub
--   => Pick given keys nub from gotten into
--   => { | given }
--   -> { | nub }
--   -> { | gotten }
-- removeDefaults = pick @given @keys @nub @from @gotten @into

class Pick :: Row Type -> RowList Type -> Row Type -> RowList Type -> Row Type -> RowList Type -> Constraint
class
  ( RowToList k keys
  , RowToList f from
  , RowToList i into
  ) <=
  Pick k keys f from i into
  | keys from -> into k f i {- where
  pick :: { | k } -> { | f } -> { | i } -}

instance pickNil :: Pick () Nil () Nil () Nil {- where
  pick :: Record () -> Record () -> Record ()
  pick _ _ = {} -}

else instance pickNoKeys ::
  ( RowToList f (Cons key a tail)
  ) =>
  Pick () Nil f (Cons key a tail) () Nil {- where
  pick :: Record () -> Record f -> Record ()
  pick _ _ = {} -}

else instance pickNoFrom ::
  ( RowToList k (Cons key a tail)
  ) =>
  Pick k (Cons key a tail) () Nil () Nil {- where
  pick :: Record k -> Record () -> Record ()
  pick _ _ = {} -}

else instance pickMiss ::
  ( RowToList f (Cons key' b fromTail)
  , Pick k (Cons key a keyTail) ft fromTail i into
  ) =>
  Pick k (Cons key a keyTail) f (Cons key' b fromTail) i into {- where
  pick :: Record k -> Record f -> Record i
  pick = pick @k @(Cons key a keyTail) @ft @fromTail @i @into -}

else instance pickKeyFound ::
  ( Pick kt keyTail ft fromTail it intoTail
  , RowToList k (Cons key a keyTail)
  , RowToList f (Cons key a fromTail)
  , RowToList i (Cons key a intoTail)
  ) =>
  Pick k (Cons key a keyTail) f (Cons key a fromTail) i (Cons key a intoTail) {- where
  pick :: Record k -> Record f -> Record i
  pick keys from =
    let
      p = Proxy @key
      val = R.get p from
      keysT = R.delete p keys
      fromT = R.delete p from
      tail = pick @kt @keyTail @ft @fromTail @it @intoTail keysT fromT
    in
      R.insert p val tail -}

pick
  :: forall given keys all from gotten into
   . RowToList given keys
  => RowToList all from
  => RowToList gotten into
  => Pick2 keys from into
  => { | given }
  -> { | all }
  -> Proxy gotten
pick _ _ = Proxy

xpd :: { height :: Number, width :: Number, x :: Number, y :: Number }
xpd = build addDefaults { x: 100.0 }

-- px :: ?p
px :: Proxy (x :: Number)
px = pick { x: 100.0 } xpd

-- Could not match type Cons "x" Number t6 with type Nil
-- pxh :: Proxy (height :: Number)

-- Could not match type "height" with type "x"
-- pxh :: Proxy (x :: Number)

-- Could not match type "x" with type "width"
-- pxh :: Proxy (x :: Number, height :: Number, width :: Number)

-- Could not match type "height" with type "b":
-- pxh :: Proxy (x :: Number, height :: Number, width :: Number, b :: Number)

pxh :: Proxy (x :: Number, height :: Number)
pxh = pick { x: 100.0, height: 100.0 } xpd

class Pick2 :: RowList Type -> RowList Type -> RowList Type -> Constraint
class Pick2 keys from into | keys from -> into

instance pick2Nil :: Pick2 Nil Nil Nil

else instance pick2NoKeys :: Pick2 Nil (Cons key a tail) Nil

else instance pick2NoFrom :: Pick2 (Cons key a tail) Nil Nil

else instance pick2KeyFound ::
  ( Pick2 keyTail fromTail intoTail
  ) =>
  Pick2 (Cons key a keyTail) (Cons key a fromTail) (Cons key a intoTail)

else instance pick2Miss ::
  ( Pick2 (Cons key a keyTail) fromTail into
  ) =>
  Pick2 (Cons key a keyTail) (Cons key' b fromTail) into

-- ┌───────────────────────────┐
-- │ Record Builder Converters │
-- └───────────────────────────┘

mkConverter
  :: forall @sym @a tail row
   . IsSymbol sym
  => Cons sym a tail row
  => (a -> a)
  -> Builder { | row } { | row }
mkConverter = modify (Proxy @sym)

type X r = (x :: Number | r)
type Y r = (y :: Number | r)
type Width r = (width :: Number | r)
type Height r = (height :: Number | r)

type Converter :: (Row Type -> Row Type) -> Type
type Converter f = forall r. Builder { | f r } { | f r }

x :: Converter X
x = mkConverter @"x" (_ + 10.0)

y :: Converter Y
y = mkConverter @"y" (_ + 10.0)

width :: Converter Width
width = mkConverter @"width" (_ + 10.0)

height :: Converter Height
height = mkConverter @"height" (_ + 10.0)

converters :: Converter (X ++ Y ++ Width ++ Height)
converters = x <<< y <<< width <<< height

type And2
  :: (Row Type -> Row Type) -> (Row Type -> Row Type) -> (Row Type -> Row Type)
type And2 a b r = a + b + r

infixl 1 type And2 as ++

-- ┌───────┐
-- │ Tests │
-- └───────┘

rx :: { h :: Number, w :: Number, x :: Number, y :: Number }
rx = build x { x: 1.0, y: 1.0, w: 1.0, h: 1.0 }

-- > rx
-- { h: 1.0, w: 1.0, x: 11.0, y: 1.0 }

ry :: { x :: Number, y :: Number }
ry = build y { y: 1.0, x: 1.0 }

-- > ry
-- { x: 1.0, y: 11.0 }

rxy :: { width :: Number, x :: Number, y :: Number }
rxy = build (y <<< x) { y: 1.0, x: 1.0, width: 1.0 }

-- > rxy
-- { width: 1.0, x: 11.0, y: 11.0 }

rxyw :: { width :: Number, x :: Number, y :: Number }
rxyw = build (y <<< x <<< width) { y: 1.0, x: 1.0, width: 1.0 }
-- > rxyw
-- { width: 11.0, x: 11.0, y: 11.0 }

-- rxy' = build (y <<< x) { y: 1.0, width: 1.0 }
--   Type of expression lacks required label x.
