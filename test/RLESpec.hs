{-# LANGUAGE TemplateHaskell #-}
module RLESpec(runTests) where

import Test.QuickCheck

import RLE

import Data.Maybe


prop_compress_empty :: Bool
prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

prop_compress_single_char :: Char -> Bool
prop_compress_single_char c = compress [c] == [(c,1)] && uncompress (compress [c]) == Just [c]

prop_compress_uncompress :: String -> Bool
prop_compress_uncompress input =
    isJust compressedData && input == fromJust compressedData
    where
        compressedData = uncompress (compress input)

prop_compress_uncompress_char_repetitions :: Char -> Property
prop_compress_uncompress_char_repetitions c =
	forAll (repetitions_char_gen c) $ \input -> let compressedData = uncompress (compress input) in isJust compressedData && input == fromJust compressedData

prop_compress_uncompress_str_repetitions :: String -> Int -> Property
prop_compress_uncompress_str_repetitions s nbRepeat =
	length s > 0 ==> isJust compressedData && input == fromJust compressedData
	where
		input = repetitions_str s nbRepeat
		compressedData = uncompress (compress input)

repetitions_char_gen :: Char -> Gen String
repetitions_char_gen c = listOf (elements [c])

repetitions_str :: String -> Int -> String
repetitions_str str nbRepeat = take nbRepeat (cycle str)

prop_uncompress_negative_index :: Bool
prop_uncompress_negative_index = isNothing $ uncompress [('a',-1)]

prop_uncompress_null_index :: Bool
prop_uncompress_null_index = isNothing $ uncompress [('a',0)]

return []
runTests :: IO Bool
runTests = $quickCheckAll

