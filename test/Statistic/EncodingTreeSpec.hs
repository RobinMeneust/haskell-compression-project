{-# LANGUAGE TemplateHaskell #-}
module Statistic.EncodingTreeSpec(runTests) where

import Test.QuickCheck
import Statistic.EncodingTree
import Statistic.Bit
import Statistic.Huffman (calculateFrequencies, tree)
import Data.Maybe (isJust)

-- Tests for compress and uncompress

prop_compress_empty :: Bool
prop_compress_empty =
  let frequencies = calculateFrequencies ""
      mTree = tree frequencies
      compressedResult = fmap (\t -> compress (\_ -> Just t) "") mTree
  in case compressedResult of
      Just (_, bits) -> uncompress (mTree, bits) == Just ""
      _ -> False

prop_compress_single_char :: Char -> Bool
prop_compress_single_char c =
  let frequencies = calculateFrequencies [c]
      mTree = tree frequencies
      compressedResult = fmap (\t -> compress (\_ -> Just t) [c]) mTree
  in case compressedResult of
      Just (_, bits) -> uncompress (mTree, bits) == Just [c]
      _ -> False

prop_compress_uncompress :: String -> Property
prop_compress_uncompress input = (not . null) input && isJust (tree (calculateFrequencies input)) ==>
  let mTree = tree (calculateFrequencies input)
      compressedResult = fmap (\t -> compress (\_ -> Just t) input) mTree
  in case compressedResult of
      Just (_, bits) -> uncompress (mTree, bits) == Just input
      _ -> False

return []
runTests :: IO Bool
runTests = $quickCheckAll

