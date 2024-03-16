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

{- 
instance Arbitrary a => Arbitrary (EncodingTree a) where
    arbitrary = sized encodingTree
      where
        encodingTree 0 = EncodingLeaf <$> arbitrary <*> arbitrary
        encodingTree n = oneof
            [ EncodingLeaf <$> arbitrary <*> arbitrary
            , EncodingNode <$> arbitrary <*> subtree <*> subtree
            ]
          where
            subtree = encodingTree (n `div` 2)

-- Property: Encoding a symbol and then decoding it should yield the original symbol
prop_EncodeDecode :: EncodingTree Char -> Char -> Property
prop_EncodeDecode tree symbol =
  let encoded = encode tree symbol
      decoded = case encoded of
                  Just bits -> fmap (\[x] -> x) (decode tree bits)  -- Decode a single symbol
                  Nothing   -> Nothing
  in Just symbol === decoded

-- Property: Encoding a symbol and then decoding it should yield the original symbol
-- when using a tree containing only leaf nodes
prop_EncodeDecodeLeafTree :: Char -> Property
prop_EncodeDecodeLeafTree symbol =
  let tree = EncodingLeaf 1 symbol
      encoded = encode tree symbol
      decoded = case encoded of
                  Just bits -> fmap (\[x] -> x) (decode tree bits)  -- Decode a single symbol
                  Nothing   -> Nothing
  in Just symbol === decoded

-- Property: Encoding a symbol and then decoding it should yield the original symbol
-- when using a tree containing only one node
prop_EncodeDecodeSingleNodeTree :: Char -> Property
prop_EncodeDecodeSingleNodeTree symbol =
  let tree = EncodingNode 1 (EncodingLeaf 1 symbol) (EncodingLeaf 1 symbol)
      encoded = encode tree symbol
      decoded = case encoded of
                  Just bits -> decodeOnce tree bits  -- Decode a single symbol
                  Nothing   -> Nothing
  in Just symbol === fmap fst decoded


-- Property: The decoding of a list of symbols should result in the same list
prop_DecodeEncode ::  [Char] -> Bool
prop_DecodeEncode tree symbols =
  let encoded = mapM (encode tree) symbols
      decoded = case encoded of
                  Just bits -> decode tree (concat bits)  -- Concatenate the list of bits
                  Nothing   -> Nothing
  in isNothing decoded || Just symbols == decoded
 -}


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

