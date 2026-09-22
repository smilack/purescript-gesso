module Gesso.Example.ReferenceFrames.Util (showAlign, showFit, rangeNumber) where

import Prelude

import Data.Array (range)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromMaybe)
import Gesso.Geometry (Align, Alignment(..), PreserveAspectRatio(..))

showAlign :: Align -> String
showAlign { x, y } = "x-" <> showAlignment x <> ", y-" <> showAlignment y
  where
  showAlignment = case _ of
    Min -> "min"
    Mid -> "mid"
    Max -> "max"

showFit :: PreserveAspectRatio -> String
showFit = case _ of
  None -> "none"
  Meet _ -> "meet"
  Slice _ -> "slice"

rangeNumber :: Number -> Number -> Array Number
rangeNumber start end = fromMaybe [] do
  s <- fromNumber start
  e <- fromNumber end
  pure $ map toNumber $ range s e
