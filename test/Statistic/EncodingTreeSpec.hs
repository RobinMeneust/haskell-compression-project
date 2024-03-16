{-# LANGUAGE TemplateHaskell #-}
module Statistic.EncodingTreeSpec(runTests) where

import Test.QuickCheck
import Statistic.EncodingTree
import Statistic.Bit
import Statistic.Huffman as Huffman
import Statistic.ShannonFano as ShannonFano
import Data.Maybe (isJust, fromJust, isNothing)
import Statistic.Source(orderedCounts)

-- Tests for compress and uncompress

prop_compress_empty_ShannonFano :: Bool
prop_compress_empty_ShannonFano =
  isNothing t && compressedData == []
	where
	(t, compressedData) = compress ShannonFano.tree ""

prop_compress_single_char_ShannonFano :: Char -> Bool
prop_compress_single_char_ShannonFano c =
	isJust t && isJust uncompressedData && [c] == fromJust uncompressedData
		where
		(t, compressedData) = compress ShannonFano.tree [c]
		uncompressedData = uncompress (t, compressedData)

prop_compress_uncompress_ShannonFano :: String -> Property
prop_compress_uncompress_ShannonFano input = (not . null) input && isJust (ShannonFano.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
		where
		(t, compressedData) = compress ShannonFano.tree input
		uncompressedData = uncompress (t, compressedData)

return []
runTests :: IO Bool
runTests = $quickCheckAll

