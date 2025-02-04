-- | A collection of types and functions for specifying sizes and positions.
module Gesso.Dimensions3 where

import Prelude

import Record (get, set, modify, insert, delete) as R
import Record.Builder (modify, insert, delete, build, buildFromScratch, Builder, flip, merge)
import Record.Extra (class Keys, pick)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Lacks, type (+), RowApply, class Cons, class Union, class Nub)
import Type.RowList (RowList, Nil, Cons, class RowToList, class ListToRow, class RowListAppend, class RowListNub)

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

-- | Alias just so I don't have to break long lines
type OpenRow = (Row Type -> Row Type)

-- | Like RowApply but right argument is also open
type OpenRowApply :: OpenRow -> OpenRow -> OpenRow
type OpenRowApply a b r = a + b + r

infixl 1 type OpenRowApply as ++

-- | Fields that will have scaling functions
type X r = (x :: Number | r)
type Y r = (y :: Number | r)
type Width r = (width :: Number | r)
type Height r = (height :: Number | r)

type ConvertibleFields = X ++ Y ++ Width ++ Height + ()

-- ┌─────────────────────────────┐
-- │ Default Fields + Operations │
-- └─────────────────────────────┘

-- | The values here are totally irrelevant; they're just to pad the record so
-- | that the converter can run, and they'll be removed after.
defaults :: { | ConvertibleFields }
defaults = { x: 0.0, y: 0.0, width: 0.0, height: 0.0 }

addDefaults
  :: forall partial union complete
   . Union partial ConvertibleFields union
  => Nub union complete
  => Builder { | partial } { | complete }
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

-- ┌──────────────┐
-- │ Delete class │
-- └──────────────┘

-- | Find keys in `from` that don't exist in `keys`. `from` is assumed to be a
-- | superset of `keys`, which simplifies a couple things:
-- |
-- |  - There's no need for an instance where `from` is `Nil` but `keys` is not.
-- |
-- |  - When the heads of `keys` and `from` are different, we only need to
-- |    advance `from`, because the key will be in there later.
class Delete :: RowList Type -> RowList Type -> RowList Type -> Constraint
class Delete keys from diff | keys from -> diff

instance deleteEmpty :: Delete Nil Nil Nil

else instance deleteNoMoreKeys :: Delete Nil from from

else instance deleteDelKey ::
  ( Delete keyTail fromTail diff
  ) =>
  Delete (Cons key a keyTail) (Cons key a fromTail) diff

else instance deleteKeepKey ::
  ( Delete (Cons key a keyTail) fromTail diffTail
  ) =>
  Delete (Cons key a keyTail) (Cons key' b fromTail) (Cons key' b diffTail)

-- | Given two records, find the set of fields in `all` that do not appear in
-- | `required`. In theory the fields should be usable to delete those keys from
-- | `all`.
diffproxy
  :: forall keys required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Delete keys from diff
  => RowToList filler diff
  => { | required }
  -> { | all }
  -> Proxy filler
diffproxy _ _ = Proxy

-- | Given two records, confirm that the fields in `required` are all present in
-- | `all` by calculating the differences `diff = from - keys`
-- | (`d = all - required`) and `keys' = from - diff` (`extracted = all - d`).
extproxy
  :: forall keys required from all diff keys' extracted
   . RowToList required keys
  => RowToList all from
  => Delete keys from diff
  => Delete diff from keys'
  => RowToList extracted keys'
  => { | required }
  -> { | all }
  -> Proxy extracted
extproxy _ _ = Proxy

extract
  :: forall keys required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Delete keys from diff
  => Union required filler all
  => Keys keys
  => { | required }
  -> { | all }
  -> { | required }
extract _ = pick

del
  :: forall keys required from all diff filler
   . RowToList required keys
  => RowToList all from
  => Delete keys from diff
  => RowToList filler diff
  => Union filler required all
  => Keys diff
  => { | required }
  -> { | all }
  -> { | filler }
del _ = pick

-- ┌──────────────┬───────┐
-- │ Delete Tests │ Proxy │
-- └──────────────┴───────┘

-- setup
xPlusDefaults :: { | ConvertibleFields }
xPlusDefaults = build addDefaults { x: 100.0 }

othersPlusDefaults :: { a :: String, b :: Boolean, c :: Int | ConvertibleFields }
othersPlusDefaults = build addDefaults { a: "hello", b: true, c: (-9) }

-- find fields in r2 but not r1
diffX :: Proxy (height :: Number, width :: Number, y :: Number)
diffX = diffproxy { x: 100.0 } xPlusDefaults

diffHeightX :: Proxy (width :: Number, y :: Number)
diffHeightX = diffproxy { x: 100.0, height: 100.0 } xPlusDefaults

diffOthers :: Proxy (height :: Number, width :: Number, x :: Number, y :: Number)
diffOthers = diffproxy { a: "", b: false, c: 0 } othersPlusDefaults

-- find fields in r1 but not r2
extX :: Proxy (x :: Number)
extX = extproxy { x: 100.0 } xPlusDefaults

extHeightX :: Proxy (x :: Number, height :: Number)
extHeightX = extproxy { x: 100.0, height: 100.0 } xPlusDefaults

extOthers :: Proxy (a :: String, b :: Boolean, c :: Int)
extOthers = extproxy { a: "", b: false, c: 0 } othersPlusDefaults

-- ┌──────────────┬────────┐
-- │ Delete Tests │ Record │
-- └──────────────┴────────┘

delXrec :: { height :: Number, width :: Number, y :: Number }
delXrec = del { x: 100.0 } xPlusDefaults

delHeightXRec :: { width :: Number, y :: Number }
delHeightXRec = del { x: 100.0, height: 100.0 } xPlusDefaults

delOthersRec :: { height :: Number, width :: Number, x :: Number, y :: Number }
delOthersRec = del { a: "", b: false, c: 0 } othersPlusDefaults

-- find fields in r1 but not r2
extXR :: { x :: Number }
extXR = extract { x: -1.0 } xPlusDefaults

extHeightXR :: { x :: Number, height :: Number }
extHeightXR = extract { x: -1.0, height: -1.0 } xPlusDefaults

extOthersR :: { a :: String, b :: Boolean, c :: Int }
extOthersR = extract { a: "", b: false, c: 0 } othersPlusDefaults

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
