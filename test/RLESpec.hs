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
    isJust uncompressedData && input == fromJust uncompressedData
    where
        uncompressedData = uncompress (compress input)

prop_compress_uncompress_char_repetitions :: Char -> Property
prop_compress_uncompress_char_repetitions c =
    forAll (repetitions_char_gen c) $ \input -> let uncompressedData = uncompress (compress input) in isJust uncompressedData && input == fromJust uncompressedData

prop_compress_uncompress_str_repetitions :: String -> Int -> Property
prop_compress_uncompress_str_repetitions s nbRepeat =
    length s > 0 ==> isJust uncompressedData && input == fromJust uncompressedData
    where
        input = repetitions_str s nbRepeat
        uncompressedData = uncompress (compress input)

repetitions_char_gen :: Char -> Gen String
repetitions_char_gen c = listOf (elements [c])

repetitions_str :: String -> Int -> String
repetitions_str str nbRepeat = take nbRepeat (cycle str)

prop_uncompress_negative_index :: Bool
prop_uncompress_negative_index = isNothing $ uncompress [('a',-1)]

prop_uncompress_null_index :: Bool
prop_uncompress_null_index = isNothing $ uncompress [('a',0)]

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

