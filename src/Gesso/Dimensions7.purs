module Gesso.Dimensions7 where

-- module Main where

import Prelude
-- import TryPureScript (Doc, render, withConsole, p, text, link, list, h3, h4, code)

-- import Data.Either (either)
-- import Data.Foldable (fold)
-- import Data.Time.Duration (Milliseconds(..))
-- import Data.Traversable (foldMap)
-- import Effect (Effect)
-- import Effect.Aff (Aff, launchAff_)
-- import Effect.Class (liftEffect)
-- import Effect.Console (log)
-- import Test.Spec (describe, it, SpecT)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.Reporter (consoleReporter)
-- import Test.Spec.Result (Result(..))
-- import Test.Spec.Runner (runSpecT, defaultConfig)
-- import Test.Spec.Tree (Tree(..))

import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Tuple.Nested ((/\))
import Record (get, delete) as Record
import Record.Builder (Builder, buildFromScratch, nub, merge, modify, build)
import Record.Builder (insert) as Builder
import Record.Extra (pick, class Keys)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Lacks, class Nub, class Union)
import Type.RowList (RowList, class RowToList, Cons, Nil)

-- main :: Effect Unit
-- main = do
--   render description
--   runTests <- runSpecT defaultConfig [ consoleReporter ] testSuite
--   launchAff_ do
--     results <- runTests
--     liftEffect $ render =<< withConsole (log $ printResults results)

--
-- Conversion functions
--

type Conversions =
  { x :: Number -> Number
  , y :: Number -> Number
  }

convs :: Conversions
convs =
  { x: (_ + 100.0)
  , y: (_ + 200.0)
  }

--
-- Method 1
--

-- | Calling convert1' requires annotations:
-- |
-- |     convert1' @"x1" @"x2" @"y1" @"y2" { x1: 1, y1: 1 }
-- |
-- | I tried adding constraints to move the symbols into the type signature:
-- |
-- |     => Prim.Symbol.Compare "x1" x1 x1ord
-- |     => Type.Data.Ordering.Equals EQ x1ord True
-- |     => Type.Data.Symbol.Equals "x1" x1 True
-- |
-- | but none of those seemed to make a difference. This alias:
-- |
-- |     convert1 = convert1' @"x1" @"x2" @"y1" @"y2"
-- |
-- | works but gives a warning for lacking a type declaration, but the
-- | compiler-suggested type -- which is essentially the same as
-- | `convert1LiteralSymbols` below -- won't compile.
convert1 = convert1' @"x1" @"x2" @"y1" @"y2"

type ConvertableFields = (x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number)

convert1'
  :: forall
       row union complete
       lst r'
       @x1 tx1
       @x2 tx2
       @y1 ty1
       @y2 ty2
   . Union row ConvertableFields union
  => Nub union complete
  -- x1
  => IsSymbol x1
  => Cons x1 Number tx1 complete
  -- x2
  => IsSymbol x2
  => Cons x2 Number tx2 complete
  -- y1
  => IsSymbol y1
  => Cons y1 Number ty1 complete
  -- y2
  => IsSymbol y2
  => Cons y2 Number ty2 complete
  -- pick
  => RowToList row lst
  => Keys lst
  => Union row r' complete
  -- params
  => { | row }
  -> { | row }
convert1' r = pick $ build
  (my2 <<< my1 <<< mx2 <<< mx1 <<< merge r)
  { x1: 0.0, y1: 0.0, x2: 0.0, y2: 0.0 }
  where
  mx1 = modify (Proxy @x1) convs.x
  mx2 = modify (Proxy @x2) convs.x
  my1 = modify (Proxy @y1) convs.y
  my2 = modify (Proxy @y2) convs.y

-- | This is roughly the type that the compiler suggests for `convert1`, which
-- | doesn't compile.
-- convert1LiteralSymbols
--   :: forall row union complete lst r' tx1 tx2 ty1 ty2
--   . Union row ConvertableFields union
--   => Nub union complete
--   -- x1
--   => IsSymbol "x1"
--   => Cons "x1" Number tx1 complete
--   -- x2
--   => IsSymbol "x2"
--   => Cons "x2" Number tx2 complete
--   -- y1
--   => IsSymbol "y1"
--   => Cons "y1" Number ty1 complete
--   -- y2
--   => IsSymbol "y2"
--   => Cons "y2" Number ty2 complete
--   -- pick
--   => RowToList row lst
--   => Keys lst
--   => Union row r' complete
--   -- params
--   => { | row }
--   -> { | row }
-- convert1LiteralSymbols r = pick $ build
--   (my2 <<< my1 <<< mx2 <<< mx1 <<< merge r)
--   { x1: 0.0, y1: 0.0, x2: 0.0, y2: 0.0 }
--   where
--   mx1 = modify (Proxy @"x1") convs.x
--   mx2 = modify (Proxy @"x2") convs.x
--   my1 = modify (Proxy @"y1") convs.y
--   my2 = modify (Proxy @"y2") convs.y

--
-- Method 2
--

convert2 :: forall r rl. RowToList r rl => ConvertableRecord rl r Number => { | r } -> { | r }
convert2 = convert2' (toConvMap convs)

convert2' :: forall rl r a. RowToList r rl => ConvertableRecord rl r a => Map String (a -> a) -> { | r } -> { | r }
convert2' m r = buildFromScratch (convertRec @rl m r)

toConvMap :: Conversions -> Map String (Number -> Number)
toConvMap { x, y } = Map.fromFoldable
  [ "x1" /\ x
  , "x2" /\ x
  , "y1" /\ y
  , "y2" /\ y
  ]

-- | Alias for three constraints that appear together a lot
class CanInsert :: Symbol -> Type -> Row Type -> Row Type -> Constraint
class (IsSymbol k, Cons k a t r, Lacks k t) <= CanInsert k a t r

instance (IsSymbol k, Cons k a t r, Lacks k t) => CanInsert k a t r

class ConvertableRecord :: RowList Type -> Row Type -> Type -> Constraint
class (RowToList r rl) <= ConvertableRecord rl r a | rl -> r where
  convertRec :: Map String (a -> a) -> { | r } -> Builder {} { | r }

instance convertableRecEmpty :: ConvertableRecord Nil () a where
  convertRec _ _ = nub

instance convertableRecConvertableType ::
  ( RowToList r (Cons k a tl)
  , CanInsert k a t r
  , ConvertableRecord tl t a
  ) =>
  ConvertableRecord (Cons k a tl) r a where
  convertRec m r = convertRecConvType @k m r <<< convertRecRecur @tl @k m r

else instance convertableRecOtherType ::
  ( RowToList r (Cons k v tl)
  , CanInsert k v t r
  , ConvertableRecord tl t a
  ) =>
  ConvertableRecord (Cons k v tl) r a where
  convertRec m r = convertRecOtherType @k r <<< convertRecRecur @tl @k m r

convertRecConvType
  :: forall @k a t r
   . CanInsert k a t r
  => Map String (a -> a)
  -> { | r }
  -> Builder { | t } { | r }
convertRecConvType m = insert @k <<< fromMaybe identity (lookup @k m) <<< get @k

convertRecOtherType
  :: forall @k a t r
   . CanInsert k a t r
  => { | r }
  -> Builder { | t } { | r }
convertRecOtherType = insert @k <<< get @k

convertRecRecur
  :: forall @tl @k a v t r
   . RowToList t tl
  => ConvertableRecord tl t a
  => CanInsert k v t r
  => Map String (a -> a)
  -> { | r }
  -> Builder {} { | t }
convertRecRecur m = convertRec @tl m <<< delete @k

--
-- Aliases for functions that use proxies
--

lookup :: forall @k a. IsSymbol k => Map String (a -> a) -> Maybe (a -> a)
lookup = Map.lookup $ reflectSymbol $ Proxy @k

insert :: forall @k a t r. CanInsert k a t r => a -> Builder { | t } { | r }
insert = Builder.insert $ Proxy @k

get :: forall @k a t r. CanInsert k a t r => { | r } -> a
get = Record.get $ Proxy @k

delete :: forall @k a t r. CanInsert k a t r => { | r } -> { | t }
delete = Record.delete $ Proxy @k

-- --
-- -- Description
-- --

-- description :: Doc
-- description = fold
--   [ h3 (text "Goal: apply functions to specific fields, if they exist, in an arbitrary record")
--   , p (text "For this example, it should:")
--   , list
--       [ text "Accept any record"
--       , text "Which may or may not contain the fields " <> code (text "x1") <> text ", " <> code (text "y1") <> text ", " <> code (text "x2") <> text ", and " <> code (text "y2") <> text " (in practice there will be several more)"
--       , text "If the record has any of those fields, and they are " <> code (text "Number") <> text "s:"
--       , text "Return a copy of the record with those fields modified"
--       ]
--   , h4 (text "Method 1: no typeclass")
--   , p (text "Merge the record with a default record that contains all the fields, apply all of the update functions, and then remove the fields that weren't in the input record")
--   , p (text "Pros:")
--   , list
--       [ text "More clear/less abstract"
--       , text "Would allow convertable fields to have different types"
--       ]
--   , p (text "Cons:")
--   , list
--       [ text "Lots more boilerplate per field/many more changes to add or remove fields"
--       , text "Implementation is dependent on the fields needed"
--       , text "Incredibly long type signature which will get much longer with additional fields"
--       , text "Not sure how to make it work without a function without a type signature (see " <> code (text "convert1") <> text ")"
--       ]
--   , p (text "Not sure:")
--   , list
--       [ text "Type error if field is present but not " <> code (text "Number") <> text ": prevents mistakes with different numeric types"
--       ]
--   , p (link ("https://gist.github.com/smilack/340ffbeeb8e135cd7f608e020634668a#file-readme-md") (text "A few more notes on how I got to this solution"))
--   , h4 (text "Method 2: typeclass")
--   , p (text "Given a record and a " <> code (text "Map String (a -> a)") <> text ", walk through each field in the record. If a value is of type " <> code (text "a") <> text ", look for a function in the map with the same key. If it exists, apply the function to the value.")
--   , p (text "Pros:")
--   , list
--       [ text "Doesn't require writing every field explicitly in type signature"
--       , text "Doesn't require function without type signature (see " <> code (text "convert1") <> text ")"
--       , text "Implementation independent of fields"
--       , text "One-line change to add or remove fields"
--       ]
--   , p (text "Cons:")
--   , list
--       [ text "More abstract/harder to understand"
--       , text "All convertable fields must have same type"
--       ]
--   , p (text "Not sure:")
--   , list
--       [ text "No type error if field is present but not " <> code (text "Number") <> text ": allows more flexibility in inputs"
--       ]
--   , h3 (text "Tests:")
--   ]

-- --
-- -- Tests
-- --

-- testSuite :: SpecT Aff Unit Effect Unit
-- testSuite = do
--   describe "Only convertable fields:" do
--     describe "x1:" do
--       let
--         i = { x1: 1.0 }
--         o = { x1: 101.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--     describe "y2:" do
--       let
--         i = { y2: 1.0 }
--         o = { y2: 201.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--     describe "x1, y1:" do
--       let
--         i = { x1: 1.0, y1: 1.0 }
--         o = { x1: 101.0, y1: 201.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--     describe "x1, y1, x2, y2:" do
--       let
--         i = { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 }
--         o = { x1: 101.0, y1: 201.0, x2: 101.0, y2: 201.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--   describe "Extra fields:" do
--     describe "x1, y1, z1:" do
--       let
--         i = { x1: 1.0, y1: 1.0, z1: 1.0 }
--         o = { x1: 101.0, y1: 201.0, z1: 1.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--     describe "x0, x1, x2:" do
--       let
--         i = { x0: 1.0, x1: 1.0, x2: 1.0 }
--         o = { x0: 1.0, x1: 101.0, x2: 101.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--   describe "No convertable fields:" do
--     describe "a, b, c:" do
--       let
--         i = { a: 1.0, b: 1.0, c: 1.0 }
--         o = { a: 1.0, b: 1.0, c: 1.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--     describe "(empty):" do
--       let
--         i = {}
--         o = {} 
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--   describe "Heterogeneous:" do
--     describe "xLabel (String), x1, yLabel (String), y1:" do
--       let
--         i = { xLabel: "a", x1: 1.0, yLabel: "a", y1: 1.0 }
--         o = { xLabel: "a", x1: 101.0, yLabel: "a", y1: 201.0 }
--       it "Method 1" $ convert1 i `shouldEqual` o
--       it "Method 2" $ convert2 i `shouldEqual` o

--   describe "Type mismatch (Method 2 only):" do
--     describe "x1 (String), y1 (Int):" do
--       it "Method 2" $ { x1: "a", y1: 1 } `shouldEqual` { x1: "a", y1: 1 }

-- printResults :: Array (Tree String Void Result) -> String
-- printResults = append "--- Tests ---\n" <<< foldMap (go "")
--   where
--   go prefix = append ("\n" <> prefix) <<< case _ of
--     Node enc at -> either identity show enc <> foldMap (go $ prefix <> "  ") at <> "\n"
--     Leaf n (Just a) ->
--       case a of
--         Success _ _ -> "✓  " <> n
--         -- Success _ (Milliseconds t) -> "✓  " <> n <> " (" <> show t <> " ms)"
--         Failure _ -> " ✗ " <> n
--     Leaf n Nothing -> "(Nothing) " <> n