{-# LANGUAGE TemplateHaskell #-}
module Statistic.ShannonFanoSpec(runTests) where

import Test.QuickCheck

import Statistic.ShannonFano()
import Statistic.EncodingTree

import Data.Maybe

-- WILL DEPEND ON THE INPUT/OUTPUT OF THE (UN)COMPRESS FUNCTIONS

-- prop_compress_empty :: Bool
-- prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

-- prop_compress_single_char :: Char -> Bool
-- prop_compress_single_char c = compress [c] == [(0,c)] && uncompress (compress [c]) == Just [c]

-- prop_compress_uncompress :: String -> Bool
-- prop_compress_uncompress input =
--     isJust output && input == fromJust output
--     where
--         output = uncompress (compress input)

-- prop_compress_uncompress_char_repetitions :: Char -> Property
-- prop_compress_uncompress_char_repetitions c =
-- 	forAll (repetitions_char_gen c) $ \input -> let output = uncompress (compress input) in isJust output && input == fromJust output

-- prop_compress_uncompress_str_repetitions :: String -> Int -> Property
-- prop_compress_uncompress_str_repetitions s nbRepeat =
-- 	length s > 0 ==> isJust output && input == fromJust output
-- 	where
-- 		input = repetitions_str s nbRepeat
-- 		output = uncompress (compress input)

-- repetitions_char_gen :: Char -> Gen String
-- repetitions_char_gen c = listOf (elements [c])

-- repetitions_str :: String -> Int -> String
-- repetitions_str str nbRepeat = take nbRepeat (cycle str)

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


-- Property: The length of the encoded bit list should be less than or equal to
-- the length of the original list of symbols
prop_EncodedLength :: EncodingTree Char -> [Char] -> Bool
prop_EncodedLength tree symbols =
  let encoded = mapM (encode tree) symbols
  in maybe False (\bits -> length bits <= length symbols) encoded

-- Property: The decoding of a list of symbols should result in the same list
prop_DecodeEncode :: EncodingTree Char -> [Char] -> Bool
prop_DecodeEncode tree symbols =
  let encoded = mapM (encode tree) symbols
      decoded = case encoded of
                  Just bits -> decode tree (concat bits)  -- Concatenate the list of bits
                  Nothing   -> Nothing
  in Just symbols == decoded


return []
runTests :: IO Bool
runTests = $quickCheckAll