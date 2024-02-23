{-# LANGUAGE TemplateHaskell #-}
module LZ78Spec(runTests) where

import Test.QuickCheck

import LZ.LZ78

import Data.Maybe


prop_compress_empty :: Bool
prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

prop_compress_single_char :: Char -> Bool
prop_compress_single_char c = compress [c] == [(0,c)] && uncompress (compress [c]) == Just [c]

prop_compress_uncompress :: String -> Bool
prop_compress_uncompress input =
    isJust output && input == fromJust output
    where
        output = uncompress (compress input)

prop_compress_uncompress_char_repetitions :: Char -> Property
prop_compress_uncompress_char_repetitions c =
	forAll (repetitions_char_gen c) $ \input -> let output = uncompress (compress input) in isJust output && input == fromJust output

prop_compress_uncompress_str_repetitions :: String -> Int -> Property
prop_compress_uncompress_str_repetitions s nbRepeat =
	length s > 0 ==> isJust output && input == fromJust output
	where
		input = repetitions_str s nbRepeat
		output = uncompress (compress input)

repetitions_char_gen :: Char -> Gen String
repetitions_char_gen c = listOf (elements [c])

repetitions_str :: String -> Int -> String
repetitions_str str nbRepeat = take nbRepeat (cycle str)

-- prop_invalid_input_uncompress :: 
-- prop_invalid_input_uncompress


return []
runTests :: IO Bool
runTests = $quickCheckAll

