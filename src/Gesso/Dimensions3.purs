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

-- ┌──────────────────────────┐
-- │ Dimension + Helper Types │
-- └──────────────────────────┘

type OpenRow = (Row Type -> Row Type)

type OpenRowApply :: OpenRow -> OpenRow -> OpenRow
type OpenRowApply a b r = a + b + r

infixl 1 type OpenRowApply as ++

type X r = (x :: Number | r)
type Y r = (y :: Number | r)
type Width r = (width :: Number | r)
type Height r = (height :: Number | r)

type ConvertibleFields = X ++ Y ++ Width ++ Height + ()

-- ┌─────────────────────────────┐
-- │ Default Fields + Operations │
-- └─────────────────────────────┘

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

class Pick :: RowList Type -> RowList Type -> RowList Type -> Constraint
class Pick keys from into | keys from -> into

instance pickNil :: Pick Nil Nil Nil

else instance pickNoKeys :: Pick Nil (Cons key a tail) Nil

else instance pickNoFrom :: Pick (Cons key a tail) Nil Nil

else instance pickKeyFound ::
  ( Pick keyTail fromTail intoTail
  ) =>
  Pick (Cons key a keyTail) (Cons key a fromTail) (Cons key a intoTail)

else instance pickMiss ::
  ( Pick (Cons key a keyTail) fromTail into
  ) =>
  Pick (Cons key a keyTail) (Cons key' b fromTail) into

pickprox
  :: forall given keys all from into
   . RowToList given keys
  => RowToList all from
  => RowToList given into
  => Pick keys from into
  => { | given }
  -> { | all }
  -> Proxy given
pickprox _ _ = Proxy

-- pick
--   :: forall given keys all from into
--    . RowToList given keys
--   => RowToList all from
--   => RowToList given into
--   => Pick keys from into
--   => { | given }
--   -> { | all }
--   -> { | given }
-- pick = ?e

-- ┌─────────────────────┐
-- │ Default Field Tests │
-- └─────────────────────┘

xPlusDefaults :: { height :: Number, width :: Number, x :: Number, y :: Number }
xPlusDefaults = build addDefaults { x: 100.0 }

pickX :: Proxy (x :: Number)
pickX = pickprox { x: 100.0 } xPlusDefaults

-- Could not match type Cons "x" Number t6 with type Nil
-- pickHeightX :: Proxy (height :: Number)
-- Could not match type "height" with type "x"
-- pickHeightX :: Proxy (x :: Number)
-- Could not match type "x" with type "width"
-- pickHeightX :: Proxy (x :: Number, height :: Number, width :: Number)
-- Could not match type "height" with type "b":
-- pickHeightX :: Proxy (x :: Number, height :: Number, width :: Number, b :: Number)
pickHeightX :: Proxy (x :: Number, height :: Number)
pickHeightX = pickprox { x: 100.0, height: 100.0 } xPlusDefaults

-- ┌───────────────────────────┐
-- │ Record Builder Converters │
-- └───────────────────────────┘

type Converter :: (Row Type -> Row Type) -> Type
type Converter f = forall r. Builder { | f r } { | f r }

mkConverter
  :: forall @sym @a tail row
   . IsSymbol sym
  => Cons sym a tail row
  => (a -> a)
  -> Builder { | row } { | row }
mkConverter = modify (Proxy @sym)

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

-- ┌─────────────────┐
-- │ Converter Tests │
-- └─────────────────┘

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
