{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map.Map a Int
occurrences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy xs = sum [-p * logBase 2 p | p <- probabilities]
  where
    counts = occurrences xs
    totalCount = sum $ Map.elems counts
    probabilities = map (\(_, c) -> fromIntegral c / fromIntegral totalCount) $ Map.toList counts


-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts = sortBy (flip compare `on` snd) . Map.toList . occurrences
