-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions3 where

import Prelude

import Record (get, set, modify, insert, delete) as R
import Record.Builder (modify, insert, delete, build, buildFromScratch, Builder)
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

type Converter :: Symbol -> Type -> Type
type Converter sym a =
  forall tail row
   . Cleave a tail row (Proxy sym)
  => Record row
  -> Record row

converter :: forall @sym @a. (a -> a) -> Converter sym a
converter f rec =
  let
    x = get @a (Proxy @sym) rec
    rec' = uncon @a (Proxy @sym) rec
    rec'' = con @a (Proxy @sym) (f x) rec'
  in
    rec''

builderConv
  :: forall @sym @a tail row
   . Cleave a tail row (Proxy sym)
  => (a -> a)
  -> Builder { | row } { | row }
builderConv = modify2 (Proxy @sym)

conv2
  :: forall sym a tail row
   . IsSymbol sym
  => Cons sym a tail row
  => (a -> a)
  -> { | row }
  -> { | row }
conv2 = R.modify (Proxy @sym)

builderConv2
  :: forall sym a tail row
   . IsSymbol sym
  => Cons sym a tail row
  => (a -> a)
  -> Builder { | row } { | row }
builderConv2 = modify (Proxy @sym)

converters
  :: { x :: Converter "x" Number
     , y :: Converter "y" Number
     , width :: Converter "width" Number
     , height :: Converter "height" Number
     }
converters =
  { x: converter @"x" (_ + 1.0)
  , y: converter @"y" (_ + 1.0)
  , width: converter @"width" (_ + 1.0)
  , height: converter @"height" (_ + 1.0)
  }

rx = converters.x { x: 1.0, y: 1.0, w: 1.0, h: 1.0 }
ry = converters.y { y: 1.0, x: 1.0 }

-- rxy = converters.y $ converters.x { y: 1.0, x: 1.0, width: 1.0 }

-- | Like Prim.Row.Cons but with operations to get/insert/delete the field
class Cleave :: Type -> Row Type -> Row Type -> Type -> Constraint
class Cleave a tail row symp | a symp -> tail row where
  get :: symp -> { | row } -> a
  con :: symp -> a -> { | tail } -> { | row }
  uncon :: symp -> { | row } -> { | tail }
  con2 :: symp -> a -> Builder { | tail } { | row }
  uncon2 :: symp -> Builder { | row } { | tail }
  modify2 :: symp -> (a -> a) -> Builder { | row } { | row }

instance
  ( IsSymbol sym
  , Cons sym a tail row
  , Lacks sym tail
  ) =>
  Cleave a tail row (Proxy sym)
  where
  get :: Proxy sym -> { | row } -> a
  get = R.get

  con :: Proxy sym -> a -> { | tail } -> { | row }
  con = R.insert

  uncon :: Proxy sym -> { | row } -> { | tail }
  uncon = R.delete

  con2 :: Proxy sym -> a -> Builder { | tail } { | row }
  con2 = insert

  uncon2 :: Proxy sym -> Builder { | row } { | tail }
  uncon2 = delete

  modify2 :: Proxy sym -> (a -> a) -> Builder { | row } { | row }
  modify2 = modify