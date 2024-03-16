{-# LANGUAGE TemplateHaskell #-}
module Statistic.SourceSpec(runTests) where

import Test.QuickCheck
import Statistic.Source

import qualified Data.Map as Map
import Data.List (nub, sortBy)
import Data.Function()

-- Property: The occurrences function correctly counts occurrences of symbols
prop_Occurrences :: String -> Bool
prop_Occurrences xs =
  let counts = occurrences xs
      actualCounts = map (\x -> (x, length (filter (== x) xs))) (nub xs) -- Use nub to remove duplicates
  in all (\(x, count) -> Map.lookup x counts == Just count) actualCounts

-- Property: The entropy function correctly calculates entropy
prop_Entropy :: String -> Property
prop_Entropy xs =
  not (null xs) ==>
    let ent = entropy xs
        counts = occurrences xs
        totalCount = sum $ Map.elems counts
        probabilities = map (\(_, c) -> fromIntegral c / fromIntegral totalCount) $ Map.toList counts
        expectedEntropy = negate $ sum [-p * logBase 2 p | p <- probabilities]
    in abs (ent - expectedEntropy) <= 0.00001 -- Tolerance for floating point comparison

-- Property: The orderedCounts function returns a list sorted by counts in descending order
prop_OrderedCounts :: String -> Bool
prop_OrderedCounts xs =
  let ordered = orderedCounts xs
      sorted = sortBy (\(_, count1) (_, count2) -> compare count2 count1) ordered -- Sort the list returned by orderedCounts
  in ordered == sorted


return []
runTests :: IO Bool
runTests = $quickCheckAll
