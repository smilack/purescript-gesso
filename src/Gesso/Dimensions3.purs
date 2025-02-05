-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions3 where

import Prelude

import Record (get, set, modify, insert, delete) as R
import Record.Builder (modify, insert, delete, build, buildFromScratch)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, type (+), RowApply, class Cons)
import Type.RowList (RowList, Nil, Cons, class RowToList, class ListToRow)

-- type Converter :: Symbol -> Type
-- type Converter prop =
--   forall tail row
--    . Lacks prop tail
--   => Cons prop Number tail row
--   => Record row
--   -> Record row

-- cx :: Converter "x"
-- cx
--   :: forall tail row listt
--    . Lacks "x" tail
--   => Cons "x" Number tail row
--   => RowToList tail listt
--   => RowToList row (Cons "x" Number listt)
--   => { | row } -- { x :: Number | tail }
--   -> { | row } -- { x :: Number | tail }
-- cx r =
--   let
--     xp = Proxy @"x"
--     x = get xp r
--     r' = delete xp r
--     r'' = insert xp x r'
--   in
--     r''

{- Current idea:
  (inspired by https://github.com/natefaubion/purescript-convertable-options/blob/main/README.md)
   Convenience: generic functions to create scaler function for a field (sym)
   Meat:
     Make function that converts a record with all fields
     Wrapper functions to add and remove defaults for unsupplied fields.

   E.g.:
     1. scale { x: ..., y: ... }
     2. adds default `w` and `h`
     3. scales all fields
     4. removes `w` and `h`
     5. returns scaled `x` and `y`
-}

type Converter :: Symbol -> Type
type Converter sym =
  forall tail row
   . Cleave tail row (Proxy sym)
  => Record row
  -> Record row

converter :: forall @sym. Converter sym
converter rec =
  let
    x = get (Proxy @sym) rec
    rec' = uncon (Proxy @sym) rec
    rec'' = con (Proxy @sym) x rec'
  in
    rec''

converters
  :: { x :: Converter "x"
     , y :: Converter "y"
     , width :: Converter "width"
     , height :: Converter "height"
     }
converters =
  { x: converter @"x"
  , y: converter @"y"
  , width: converter @"width"
  , height: converter @"height"
  }

-- rx = converters.x { x: 1.0 }
-- ry = converters.x { y: 1.0, x: 1.0 }

-- | Like Prim.Row.Cons but with operations to get/insert/delete the field
class Cleave :: Row Type -> Row Type -> Type -> Constraint
class Cleave tail row symp | symp -> tail row where
  get :: symp -> { | row } -> Number
  con :: symp -> Number -> { | tail } -> { | row }
  uncon :: symp -> { | row } -> { | tail }

instance
  ( IsSymbol sym
  , Cons sym Number tail row
  , Lacks sym tail
  ) =>
  Cleave tail row (Proxy sym)
  where
  get :: Proxy sym -> { | row } -> Number
  get = R.get

  con :: Proxy sym -> Number -> { | tail } -> { | row }
  con = R.insert

  uncon :: Proxy sym -> { | row } -> { | tail }
  uncon = R.delete