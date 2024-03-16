{-# LANGUAGE TemplateHaskell #-}
module LZ.LZ78Spec(runTests) where

import Test.QuickCheck

import LZ.LZ78

import Data.Maybe


prop_compress_empty :: Bool
prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

prop_compress_line_breaks :: Bool
prop_compress_line_breaks = compress "a\nb\n\nc\n" == [(0,'a'),(0,'\n'),(0,'b'),(2,'\n'),(0,'c'),(0,'\n')] && uncompress (compress "a\nb\n\nc\n") == Just "a\nb\n\nc\n"

prop_compress_special_characters :: Bool
prop_compress_special_characters = compress "\0\5\123e\a@\0\\" == [(0,'\0'),(0,'\5'),(0,'\123'),(0,'e'),(0,'\a'),(0,'@'), (1,'\\')] && uncompress (compress "\0\5\123e\a@\\") == Just "\0\5\123e\a@\\"

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

prop_uncompress_negative_index :: Bool
prop_uncompress_negative_index = isNothing $ uncompress [(-1,'a')]

prop_uncompress_too_big_index :: Bool
prop_uncompress_too_big_index = isNothing $ uncompress [(1,'a')]


return []
runTests :: IO Bool
runTests = $quickCheckAll

