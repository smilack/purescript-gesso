module Gesso.Dimensions6 where

-- module Main where

import Prelude
-- import TryPureScript (render, withConsole)

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Traversable (foldMap)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
-- import Test.Spec (pending, describe, it)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.Reporter (consoleReporter)
-- import Test.Spec.Result (Result(..))
-- import Test.Spec.Runner (runSpecT, defaultConfig)
-- import Test.Spec.Tree (Tree(..))

import Prim.Boolean (True)
import Prim.Ordering (EQ)
import Prim.Symbol (class Compare)
import Record.Builder (merge, modify, Builder, build)
import Record.Extra (pick, class Keys)
import Type.Data.Ordering (class Equals) as Ord
import Type.Data.Symbol (class Equals)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Union, class Nub)
import Type.RowList (class RowToList)

-- main :: Effect Unit
-- main = do
--   render =<< withConsole do
--     log "Conversions functions:"
--     log "  x: (_ * 2.0)"
--     log "  y: (_ * 10.0)"

--   results <- runSpecT defaultConfig [ consoleReporter ] do
--     describe "Only convertable fields:" do

--       it "{ x1: 1.0 } == { x1: 2.0 }" $
--         convert { x1: 1.0 } `shouldEqual` { x1: 2.0 }

--       it "{ y2: 1.0 } == { y2: 10.0 }" $
--         convert { y2: 1.0 } `shouldEqual` { y2: 10.0 }

--       it "{ x1: 1.0, y1: 1.0 } == { x1: 2.0, y1: 10.0 }" $
--         convert { x1: 1.0, y1: 1.0 } `shouldEqual` { x1: 2.0, y1: 10.0 }

--       it "{ x1: 1.0, x2: 1.0 } == { x1: 2.0, x2: 2.0 }" $
--         convert { x1: 1.0, x2: 1.0 } `shouldEqual` { x1: 2.0, x2: 2.0 }

--       it "{ x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 } == { x1: 2.0, y1: 10.0, x2: 2.0, y2: 10.0 }" $
--         convert { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 } `shouldEqual` { x1: 2.0, y1: 10.0, x2: 2.0, y2: 10.0 }

--     describe "Extra fields:" do
--       pending ""

--     describe "No convertable fields:" do
--       pending ""

--   void $ launchAff do
--     a <- results
--     liftEffect $ render =<< withConsole (log $ printResults a)

-- printResults :: Array (Tree String Void Result) -> String
-- printResults = foldMap go
--   where
--   go = case _ of
--     Node enc at -> "\n" <> either identity show enc <> foldMap go at <> "\n"
--     Leaf n (Just a) ->
--       case a of
--         Success _ _ -> "\n ✓  " <> n
--         Failure _ -> "\n  ✗ " <>  n
--     Leaf n Nothing -> "\n (Nothing) " <> n

{-
  Main goal: convert arbitrary records between coordinate systems

  Accept any record that may or may not contain convertable fields* - but if it
    has any of those fields, then they must be a certain type.

  Return a copy of the record with conversion functions applied to only those
    fields.

  Convertable fields are:
    `x`, `y`, `x1`, `y1`, `x2`, `y2`, `width`, `w`, `height`, `h`

  ---

  Prototype: convertable fields limited to `x1`, `y1`, `x2`, `y2`
-}

type Fields :: Type -> Row Type
type Fields a = (x1 :: a, y1 :: a, x2 :: a, y2 :: a)

conversions :: { x :: Number -> Number, y :: Number -> Number }
conversions =
  { x: (_ * 2.0)
  , y: (_ * 10.0)
  }

defaults :: { | Fields Number }
defaults = { x1: 0.0, y1: 0.0, x2: 0.0, y2: 0.0 }

tst1 :: { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
tst1 = convert { x1: 1.0, y1: 1.0, x2: 1.0, y2: 1.0 }

tst2 :: { x1 :: Number }
tst2 = convert { x1: 1.0 }

convert = convert' @"x1" @"x2" @"y1" @"y2"

convert'
  :: forall
       row union complete
       lst r'
       @x1 tx1 -- x1ord
       @x2 tx2 -- x2ord
       @y1 ty1 -- y1ord
       @y2 ty2 -- y2ord
   . Union row (Fields Number) union
  => Union (Fields Number) row union
  => Nub union complete
  -- x1
  -- => Compare "x1" x1 x1ord
  -- => Ord.Equals EQ x1ord True
  => Equals "x1" x1 True
  => IsSymbol x1
  => Cons x1 Number tx1 complete
  -- x2
  -- => Compare "x2" x2 x2ord
  -- => Ord.Equals EQ x2ord True
  => Equals "x2" x2 True
  => IsSymbol x2
  => Cons x2 Number tx2 complete
  -- y1
  -- => Compare "y1" y1 y1ord
  -- => Ord.Equals EQ y1ord True
  => Equals "y1" y1 True
  => IsSymbol y1
  => Cons y1 Number ty1 complete
  -- y2
  -- => Compare "y2" y2 y2ord
  -- => Ord.Equals EQ y2ord True
  => Equals "y2" y2 True
  => IsSymbol y2
  => Cons y2 Number ty2 complete
  -- pick
  => RowToList row lst
  => Keys lst
  => Union row r' complete
  -- params
  => {- { | Fields Number }
  -> -} { | row }
  -> { | row }
convert' {- d -} r = pick $ build (my2 <<< my1 <<< mx2 <<< mx1 <<< merge r) defaults
  where
  mx1 = modify (Proxy @x1) conversions.x
  mx2 = modify (Proxy @x2) conversions.x
  my1 = modify (Proxy @y1) conversions.y
  my2 = modify (Proxy @y2) conversions.y

-- | Don't think this actually impacts the scale function
-- class HasField :: Symbol -> Symbol -> Type -> Row Type -> Constraint
-- class HasField str sym a row

-- instance
--   ( Equals str sym True
--   , IsSymbol sym
--   , Cons sym a t row
--   ) =>
--   HasField str sym a row

-- class Scalable row a

-- instance (Union row (Fields a) u, Nub u row) => Scalable row a

-- sca
--   :: forall a row u1 u2
--    . Union row (Fields a) u1
--   => Nub u1 row
--   => Union (Fields a) row u2
--   => Nub u2 row
--   => { | Fields (a -> a) }
--   -> { | row }
--   -> a
-- sca fs r = fs.x r.x

{- a0 :: Proxy (Fields Int)
a0 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy ())

a1 :: Proxy (Fields Int)
a1 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (x :: Int))

a2 :: Proxy (Fields Int)
a2 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (y :: Int))

a3 :: Proxy (Fields Int)
a3 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (x :: Int, y :: Int))

a4 :: Proxy (z :: Int | Fields Int)
a4 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (x :: Int, y :: Int, z :: Int))

a5 :: Proxy (z :: Int | Fields Int)
a5 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (z :: Int))

a6 :: Proxy (z :: Int | Fields Int)
a6 = scale
  (Proxy :: Proxy (Fields Int))
  (Proxy :: Proxy (z :: Int, x :: Int)) -}

-- a7 :: Proxy (Fields Number)
-- a7 = scale
--   (Proxy :: Proxy (Fields Int))
--   (Proxy :: Proxy (Fields Number))
