module Gesso.Dimensions6 where

-- module Main where

import Prelude
-- import TryPureScript (Doc, render, withConsole, p, text, link, list, h3)

import Data.Either (either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
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

import Record.Builder (merge, modify, build)
import Record.Extra (pick, class Keys)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Union, class Nub)
import Type.RowList (class RowToList)

{-
Full description in readme:
https://gist.github.com/smilack/340ffbeeb8e135cd7f608e020634668a#file-readme-md
-}

-- main :: Effect Unit
-- main = do
--   render description
--   runTests <- runSpecT defaultConfig [ consoleReporter ] testSuite
--   launchAff_ do
--     results <- runTests
--     liftEffect $ render =<< withConsole (log $ printResults results)

-- Conversions

type Fields :: Type -> Row Type
type Fields a = (x1 :: a, y1 :: a, x2 :: a, y2 :: a)

conversions :: { x :: Int -> Int, y :: Int -> Int }
conversions = { x: (_ + 100), y: (_ + 200) }

convert = convert' @"x1" @"x2" @"y1" @"y2"

convert'
  :: forall
       row union complete
       lst r'
       @x1 tx1
       @x2 tx2
       @y1 ty1
       @y2 ty2
   . Union row (Fields Int) union
  => Nub union complete
  -- x1
  => IsSymbol x1
  => Cons x1 Int tx1 complete
  -- x2
  => IsSymbol x2
  => Cons x2 Int tx2 complete
  -- y1
  => IsSymbol y1
  => Cons y1 Int ty1 complete
  -- y2
  => IsSymbol y2
  => Cons y2 Int ty2 complete
  -- pick
  => RowToList row lst
  => Keys lst
  => Union row r' complete
  -- params
  => { | row }
  -> { | row }
convert' r = pick $ build
  (my2 <<< my1 <<< mx2 <<< mx1 <<< merge r)
  { x1: 0, y1: 0, x2: 0, y2: 0 }
  where
  mx1 = modify (Proxy @x1) conversions.x
  mx2 = modify (Proxy @x2) conversions.x
  my1 = modify (Proxy @y1) conversions.y
  my2 = modify (Proxy @y2) conversions.y

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
--       [ text "merging the input record with a function that contains all the fields"
--       , text "applying the conversion functions to all fields in the merged record"
--       , text "removing all fields added in step 1"
--       ]
--   , p (text "Full description of how I arrived at this solution in the readme:")
--   , p
--       ( link
--           ("https://gist.github.com/smilack/340ffbeeb8e135cd7f608e020634668a#file-readme-md")
--           (text "https://gist.github.com/smilack/340ffbeeb8e135cd7f608e020634668a#file-readme-md")
--       )
--   , p (text "Better solution:")
--   , list
--       [ link
--           ("")
--           (text "TryPureScript")
--       , link
--           ("")
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
--       convert { x1: 1 }
--         `shouldEqual` { x1: 101 }

--     it "y2" $
--       convert { y2: 1 }
--         `shouldEqual` { y2: 201 }

--     it "x1, y1" $
--       convert { x1: 1, y1: 1 }
--         `shouldEqual` { x1: 101, y1: 201 }

--     it "x1, y1, x2, y2" $
--       convert { x1: 1, y1: 1, x2: 1, y2: 1 }
--         `shouldEqual` { x1: 101, y1: 201, x2: 101, y2: 201 }

--   describe "Extra fields:" do
--     it "x1, y1, z1" $
--       convert { x1: 1, y1: 1, z1: 1 }
--         `shouldEqual` { x1: 101, y1: 201, z1: 1 }

--     it "x0, x1, x2" $
--       convert { x0: 1, x1: 1, x2: 1 }
--         `shouldEqual` { x0: 1, x1: 101, x2: 101 }

--   describe "No convertable fields:" do
--     it "a, b, c" $
--       convert { a: 1, b: 1, c: 1 }
--         `shouldEqual` { a: 1, b: 1, c: 1 }

--     it "(empty)" $
--       convert {} `shouldEqual` {}

--   describe "Heterogeneous:" do
--     it "xLabel (String), x1, yLabel (String), y1" $
--       convert { xLabel: "a", x1: 1, yLabel: "a", y1: 1 }
--         `shouldEqual` { xLabel: "a", x1: 101, yLabel: "a", y1: 201 }

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
