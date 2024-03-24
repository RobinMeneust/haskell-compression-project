{-# LANGUAGE TemplateHaskell #-}
module LZ.LZWSpec(runTests) where

import Test.QuickCheck

import LZ.LZW

import Data.Maybe

import Data.Char

import Data.List

prop_compress_empty :: Bool
prop_compress_empty = compress "" == [] && uncompress (compress "") == Just ""

prop_compress_single_char :: Char -> Property
prop_compress_single_char c = ord c <= 255 ==> compress [c] == [ord c] && uncompress (compress [c]) == Just [c]

prop_compress_line_breaks :: Bool
prop_compress_line_breaks = uncompress (compress "a\nb\n\nc\n") == Just "a\nb\n\nc\n"

-- LZW can't compress/decompress character with a value greater than 255 (\1000 for instance) so it must return Nothing
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


prop_compress_uncompress_small_file :: Bool
prop_compress_uncompress_small_file = isJust uncompressedData && input == fromJust uncompressedData
    where
        input = "belle echelle !"
        uncompressedData = uncompress (compress input)

prop_compress_uncompress_medium_file :: Bool
prop_compress_uncompress_medium_file = isJust uncompressedData && input == fromJust uncompressedData
    where
        input = "Haskell est un langage de programmation fonctionnel fondé sur le lambda-calcul et la logique combinatoire.\nSon nom vient du mathématicien et logicien Haskell Curry. Il a été créé en 1990 par un comité de chercheurs en théorie des langages intéressés par les langages fonctionnels et l'évaluation paresseuse.\nLe dernier standard est Haskell 2010 : c'est une version minimale et portable du langage conçue à des fins pédagogiques et pratiques, dans un souci d'interopérabilité entre les implémentations du langage et comme base de futures extensions.\nLe langage continue d'évoluer en 2020, principalement avec GHC, constituant ainsi un standard de facto comprenant de nombreuses extensions."
        uncompressedData = uncompress (compress input)
   

return []
runTests :: IO Bool
runTests = $quickCheckAll

