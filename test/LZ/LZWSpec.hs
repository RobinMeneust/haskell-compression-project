{-# LANGUAGE TemplateHaskell #-}
module LZ.LZWSpec(runTests) where

import Test.QuickCheck

import LZ.LZW

import Data.Maybe

import Data.Char

import Data.List

prop_compress_empty :: Bool
prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

-- prop_compress_single_char :: Char -> Bool
-- prop_compress_single_char c = compress [c] == [(0,c)] && uncompress (compress [c]) == Just [c]

prop_compress_line_breaks :: Bool
prop_compress_line_breaks = uncompress (compress "a\nb\n\nc\n") == Just "a\nb\n\nc\n"

-- LZW can't compress/decompress character with a value greater than 255 (\1000 for instance) so it muste return Nothing
prop_compress_special_characters :: Bool
prop_compress_special_characters = uncompress (compress "\0\5000\123e\a@\\") == Just ("\0\\5000\123e\a@\\")


prop_compress_uncompress :: String -> Property
prop_compress_uncompress input =
	length input > 0 && isNothing (find (\c -> ord c > 255) input) ==> isJust compressedData && input == fromJust compressedData
    where
        compressedData = uncompress (compress input)

prop_compress_uncompress_char_repetitions :: Property
prop_compress_uncompress_char_repetitions =
	forAll generate_repetitions_char_ascii $ \input -> let compressedData = uncompress (compress input) in isJust compressedData && input == fromJust compressedData

prop_compress_uncompress_str_repetitions :: String -> Int -> Property
prop_compress_uncompress_str_repetitions s nbRepeat =
	length s > 0 && isNothing (find (\c -> ord c > 255) s) ==> isJust compressedData && input == fromJust compressedData
	where
		input = repetitions_str s nbRepeat
		compressedData = uncompress (compress input)

generate_repetitions_char_ascii :: Gen String
generate_repetitions_char_ascii = listOf (elements ['\0'..'\255'])


repetitions_str :: String -> Int -> String
repetitions_str str nbRepeat = take nbRepeat (cycle str)

prop_uncompress_negative_index :: Bool
prop_uncompress_negative_index = isNothing $ uncompress [(-1)]

prop_uncompress_too_big_index :: Bool
prop_uncompress_too_big_index = isNothing $ uncompress [259]


return []
runTests :: IO Bool
runTests = $quickCheckAll

