{-# LANGUAGE TemplateHaskell #-}
module Statistic.HuffmanSpec(runTests) where

import Test.QuickCheck
import Statistic.Huffman (tree)
import Statistic.EncodingTree (EncodingTree(..), count)
import Data.List (nub, sort)
import Control.Monad (liftM2)
{- 
-- Ensure that calculateFrequencies return the right frequencies for a given string
prop_calculateFrequencies_correctness :: String -> Property
prop_calculateFrequencies_correctness str =
  let frequencies = calculateFrequencies str
      correct = all (\(c, freq) -> freq == length (filter (== c) str)) frequencies
  in collect (length str) $ correct

-- ensure that the built Huffman tree has the right amount of leaf for unical characters
prop_tree_correctness :: [Char] -> Property
prop_tree_correctness str = not (null str) ==>
  let freqs = calculateFrequencies str
      mTree = tree freqs
      totalFreq = sum $ map snd freqs
      numLeaves = length $ nub str
  in case mTree of
       Just t -> let freq = count t
                     leaves = countLeaves t
                 in freq === totalFreq .&&. leaves === numLeaves
       Nothing -> property False

countLeaves :: EncodingTree a -> Int
countLeaves (EncodingLeaf _ _) = 1
countLeaves (EncodingNode _ l r) = countLeaves l + countLeaves r
 -}
return []
runTests :: IO Bool
runTests = $quickCheckAll

