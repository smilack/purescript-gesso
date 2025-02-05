-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions3 where

import Prelude

import Record (get, set, modify, insert, delete) as R
import Record.Builder (modify, insert, delete, build, buildFromScratch, Builder)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, type (+), RowApply, class Cons)
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

mkConverter :: forall @sym @a tail row. IsSymbol sym => Cons sym a tail row => (a -> a) -> Builder { | row } { | row }
mkConverter = modify (Proxy @sym)

type X r = (x :: Number | r)
type Y r = (y :: Number | r)
type Width r = (width :: Number | r)
type Height r = (height :: Number | r)

type Converter :: (Row Type -> Row Type) -> Type
type Converter f = forall r. Builder { | f r } { | f r }

x :: Converter X
x = mkConverter @"x" @Number (_ + 10.0)

y :: Converter Y
y = mkConverter @"y" @Number (_ + 10.0)

width :: Converter Width
width = mkConverter @"width" @Number (_ + 10.0)

height :: Converter Height
height = mkConverter @"height" @Number (_ + 10.0)

rx = build x { x: 1.0, y: 1.0, w: 1.0, h: 1.0 }
-- > rx
-- { h: 1.0, w: 1.0, x: 11.0, y: 1.0 }

ry = build y { y: 1.0, x: 1.0 }
-- > ry
-- { x: 1.0, y: 11.0 }

rxy = build (y <<< x) { y: 1.0, x: 1.0, width: 1.0 }
-- > rxy
-- { width: 1.0, x: 11.0, y: 11.0 }

rxyw = build (y <<< x <<< width) { y: 1.0, x: 1.0, width: 1.0 }
-- > rxyw
-- { width: 11.0, x: 11.0, y: 11.0 }

-- rxy' = build (y <<< x) { y: 1.0, width: 1.0 }
--   Type of expression lacks required label x.