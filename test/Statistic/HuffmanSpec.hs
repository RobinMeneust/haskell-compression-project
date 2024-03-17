{-# LANGUAGE TemplateHaskell #-}
module Statistic.HuffmanSpec(runTests) where

import Test.QuickCheck
import Statistic.Huffman (tree)
import Statistic.EncodingTree (EncodingTree(..), count)
import Data.List (nub, sort)
import Control.Monad (liftM2)
import Statistic.EncodingTree
import Data.Maybe

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


generate_positive_int :: Gen Int
generate_positive_int = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

prop_build_tree_empty_string :: Bool
prop_build_tree_empty_string = isNothing(tree "")

prop_build_tree_single_char_repeat :: Char -> Property
prop_build_tree_single_char_repeat c =
	forAll generate_positive_int (\occ -> tree (take occ (repeat c)) == Just (EncodingLeaf occ c))


return []
runTests :: IO Bool
runTests = $quickCheckAll

