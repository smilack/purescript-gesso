module Gesso.Dimensions7 where

-- module Main where

import Prelude
-- import TryPureScript (Doc, render, withConsole, p, text, link, list, h3)

import Data.Either (either)
import Data.Foldable (fold)
import Data.Traversable (foldMap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
import Record.Builder (Builder, buildFromScratch, nub)
import Record.Builder (insert) as Builder
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Lacks)
import Type.RowList (RowList, class RowToList, Cons, Nil)

{-
Full description in readme:
_________________________
-}

-- main :: Effect Unit
-- main = do
--   render description
--   runTests <- runSpecT defaultConfig [ consoleReporter ] testSuite
--   launchAff_ do
--     results <- runTests
--     liftEffect $ render =<< withConsole (log $ printResults results)

convert :: forall r rl. RowToList r rl => Scalable rl r Number => { | r } -> { | r }
convert = scale (toConversions fs)

scale :: forall rl r a. RowToList r rl => Scalable rl r a => Map String (a -> a) -> { | r } -> { | r }
scale m r = buildFromScratch (scaler @rl m r)

type BasicConversions =
  { x :: Number -> Number
  , y :: Number -> Number
  , width :: Number -> Number
  , height :: Number -> Number
  }

fs :: BasicConversions
fs =
  { x: (_ + 100.0)
  , y: (_ + 200.0)
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

instance scalableNil :: Scalable Nil () a where
  scaler _ _ = nub

instance scalableSameType ::
  ( RowToList r (Cons k a tl)
  , Buildable k a t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k a tl) r a where
  scaler m r = scalerSameType @k m r <<< scalerRecur @tl @k m r

else instance scalableDiffType ::
  ( RowToList r (Cons k v tl)
  , Buildable k v t r
  , Scalable tl t a
  ) =>
  Scalable (Cons k v tl) r a where
  scaler m r = scalerDiffType @k r <<< scalerRecur @tl @k m r

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

scalerRecur
  :: forall @tl @k a v t r
   . RowToList t tl
  => Scalable tl t a
  => Buildable k v t r
  => Map String (a -> a)
  -> { | r }
  -> Builder {} { | t }
scalerRecur m = scaler @tl m <<< delete @k

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

--
-- Description
--

-- description :: Doc
-- description = fold
--   [ h3 (text "Goal: convert arbitrary records between coordinate systems")
--   , p (text "It should:")
--   , list
--       [ text "Accept any record"
--       , text "Which may or may not contain the fields `x`, `y`, `x1`, `y1`, `x2`, `y2`, `width`, `w`, `height`, `h`"
--       , text "If it has any of those fields, then they must be a certain type"
--       , text "Return a copy of the record with conversion functions applied to only those fields"
--       ]
--   , p (text "For this example, convertable fields are limited to `x1`, `y1`, `x2`, `y2`")
--   , p (text "This example works by:")
--   , list
--       [ text ""
--       , text ""
--       , text ""
--       ]
--   , p (text "Full description of how I arrived at this solution in the readme:")
--   , p
--       ( link
--           ("________________________")
--           (text "________________________")
--       )
--   , p (text "Better solution:")
--   , list
--       [ link
--           ("________________________")
--           (text "TryPureScript")
--       , link
--           ("________________________")
--           (text "Readme")
--       ]
--   , h3 (text "Tests:")
--   ]

--
-- Tests
--

-- testSuite :: SpecT Aff Unit Effect Unit
-- testSuite = do
--   describe "Only convertable fields:" do
--     it "x1" $
--       convert { x1: 1.0 }
--         `shouldEqual` { x1: 101.0 }

--     it "y2" $
--       convert { y2: 1.0 }
--         `shouldEqual` { y2: 201.0 }

--     it "x1, y1" $
--       convert { x1: 1.0, y1: 1.0 }
--         `shouldEqual` { x1: 101.0, y1: 201.0 }

--     it "x1, y1, x2, y2" $
--       convert { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 }
--         `shouldEqual` { x1: 101.0, y1: 201.0, x2: 101.0, y2: 201.0 }

--   describe "Extra fields:" do
--     it "x1, y1, z1" $
--       convert { x1: 1.0, y1: 1.0, z1: 1.0 }
--         `shouldEqual` { x1: 101.0, y1: 201.0, z1: 1.0 }

--     it "x0, x1, x2" $
--       convert { x0: 1.0, x1: 1.0, x2: 1.0 }
--         `shouldEqual` { x0: 1.0, x1: 101.0, x2: 101.0 }

--   describe "No convertable fields:" do
--     it "a, b, c" $
--       convert { a: 1.0, b: 1.0, c: 1.0 }
--         `shouldEqual` { a: 1.0, b: 1.0, c: 1.0 }

--     it "(empty)" $
--       convert {} `shouldEqual` {}

--   describe "Heterogeneous:" do
--     it "xLabel (String), x1, yLabel (String), y1" $
--       convert { xLabel: "a", x1: 1.0, yLabel: "a", y1: 1.0 }
--         `shouldEqual` { xLabel: "a", x1: 101.0, yLabel: "a", y1: 201.0 }

-- printResults :: Array (Tree String Void Result) -> String
-- printResults = append "--- Tests ---\n" <<< foldMap go
--   where
--   go = append "\n" <<< case _ of
--     Node enc at -> either identity show enc <> foldMap go at <> "\n"
--     Leaf n (Just a) ->
--       "  " <> case a of
--         Success _ _ -> "✓  " <> n
--         Failure _ -> " ✗ " <> n
--     Leaf n Nothing -> "(Nothing) " <> n