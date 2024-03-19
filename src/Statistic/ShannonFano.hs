{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : Nino Hamel
-}
module Statistic.ShannonFano(tree) where

import Statistic.EncodingTree
import Data.List (sortBy)
import Data.Function (on)
import Statistic.Source(orderedCounts)

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing  -- Base case: Empty list
tree symbols = buildTree $ sortBy (flip compare `on` snd) $ orderedCounts symbols

-- | Build Shannon-Fano tree recursively
buildTree :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
buildTree [] = Nothing  -- Base case: Empty list
buildTree [(x, c)] = Just (EncodingLeaf c x)  -- Base case: Single symbol
buildTree symbols = case (leftTree, rightTree) of
  (Just left, Just right) -> Just (EncodingNode totalCount left right)
  _ -> Nothing
  where
    (leftSymbols, rightSymbols) = splitAt (length symbols `div` 2) symbols
    totalCount = sum $ map (\(_, cnt) -> cnt) symbols
    leftTree = buildTree leftSymbols
    rightTree = buildTree rightSymbols