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

cleaver :: forall @sym. Proxy sym
cleaver = Proxy

type Converter :: Symbol -> RowList Type -> Type
type Converter sym list =
  forall tail row
   . Cleave tail row (Proxy sym) (Proxy (Cons sym Number list))
  => Record row
  -> Record row

converter :: forall @sym list. Converter sym list
converter rec =
  let
    p = cleaver @sym
    x = get p rec
    rec' = uncon p rec
    rec'' = con p x rec'
  in
    rec''

-- converters
--   :: { x :: forall list. Converter "x" list
--      , y :: forall list. Converter "y" list
--      , width :: forall list. Converter "width" list
--      , height :: forall list. Converter "height" list
--      }
-- converters =
--   { x: converter @"x"
--   , y: converter @"y"
--   , width: converter @"width"
--   , height: converter @"height"
--   }

-- rx = converters.x { x: 1.0 }
-- ry = converters.x { y: 1.0, x: 1.0 }

-- | Like Prim.Row.Cons but with operations to get/insert/delete the field
class Cleave :: Row Type -> Row Type -> Type -> Type -> Constraint
class Cleave tail row symp listp | symp -> tail row listp where
  get :: symp -> { | row } -> Number
  con :: symp -> Number -> { | tail } -> { | row }
  uncon :: symp -> { | row } -> { | tail }

instance
  ( IsSymbol sym
  , Cons sym Number tail row
  , Lacks sym tail
  , RowToList tail listt
  ) =>
  Cleave tail row (Proxy sym) (Proxy (Cons sym Number listt))
  where
  get :: Proxy sym -> { | row } -> Number
  get = R.get

  con :: Proxy sym -> Number -> { | tail } -> { | row }
  con = R.insert

  uncon :: Proxy sym -> { | row } -> { | tail }
  uncon = R.delete