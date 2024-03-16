{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where

import Statistic.EncodingTree
import Statistic.Source
import Data.List (sortBy)
import Data.Map(toList)
import Data.Function (on)

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing  -- Base case: Empty list
tree symbols = buildTree $ sortBy (flip compare `on` snd) $ calculateFrequencies symbols

-- | Calculate symbol probabilities
-- probabilities :: Ord a => [a] -> [(a, Double)]
-- probabilities symbols = map (\(x, cnt) -> (x, fromIntegral cnt / total)) counts
--   where
--    counts = toList $ occurrences symbols
--    total = fromIntegral $ sum $ map snd counts

-- | Sort symbols by their probabilities
-- orderedProbabilities :: Ord a => [a] -> [(a, Double)]
-- orderedProbabilities = sortBy (compare `on` snd) . probabilities

-- | Build Shannon-Fano tree recursively
buildTree :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
buildTree [] = Nothing  -- Base case: Empty list
buildTree [(x, _)] = Just (EncodingLeaf 1 x)  -- Base case: Single symbol
buildTree symbols = case (leftTree, rightTree) of
  (Just left, Just right) -> Just (EncodingNode totalCount left right)
  _ -> Nothing
  where
    (leftSymbols, rightSymbols) = splitAt (length symbols `div` 2) symbols
    totalCount = sum $ map (\(_, cnt) -> cnt) symbols
    leftTree = buildTree leftSymbols
    rightTree = buildTree rightSymbols