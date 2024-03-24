{-# LANGUAGE TemplateHaskell #-}
module Statistic.EncodingTreeSpec(runTests) where

import Test.QuickCheck
import Statistic.EncodingTree
-- import Statistic.Bit
import Statistic.Huffman as Huffman
import Statistic.ShannonFano as ShannonFano
import Data.Maybe (isJust, fromJust, isNothing)
import Statistic.Source(orderedCounts)
import System.IO -- To read files

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
prop_compress_uncompress_ShannonFano input = isJust (ShannonFano.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        (t, compressedData) = compress ShannonFano.tree input
        uncompressedData = uncompress (t, compressedData)


prop_compress_uncompress_ShannonFano_small_file :: Property
prop_compress_uncompress_ShannonFano_small_file = isJust (ShannonFano.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        input = "belle echelle !"
        (t, compressedData) = compress ShannonFano.tree input
        uncompressedData = uncompress (t, compressedData)

prop_compress_uncompress_ShannonFano_medium_file :: Property
prop_compress_uncompress_ShannonFano_medium_file = isJust (ShannonFano.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        input = "Haskell est un langage de programmation fonctionnel fondé sur le lambda-calcul et la logique combinatoire.\nSon nom vient du mathématicien et logicien Haskell Curry. Il a été créé en 1990 par un comité de chercheurs en théorie des langages intéressés par les langages fonctionnels et l'évaluation paresseuse.\nLe dernier standard est Haskell 2010 : c'est une version minimale et portable du langage conçue à des fins pédagogiques et pratiques, dans un souci d'interopérabilité entre les implémentations du langage et comme base de futures extensions.\nLe langage continue d'évoluer en 2020, principalement avec GHC, constituant ainsi un standard de facto comprenant de nombreuses extensions."
        (t, compressedData) = compress ShannonFano.tree input
        uncompressedData = uncompress (t, compressedData)


prop_compress_empty_Huffman :: Bool
prop_compress_empty_Huffman =
  isNothing t && compressedData == []
    where
    (t, compressedData) = compress Huffman.tree ""

prop_compress_single_char_Huffman :: Char -> Bool
prop_compress_single_char_Huffman c =
    isJust t && isJust uncompressedData && [c] == fromJust uncompressedData
        where
        (t, compressedData) = compress Huffman.tree [c]
        uncompressedData = uncompress (t, compressedData)

prop_compress_uncompress_Huffman :: String -> Property
prop_compress_uncompress_Huffman input = isJust (Huffman.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        (t, compressedData) = compress Huffman.tree input
        uncompressedData = uncompress (t, compressedData)

prop_compress_uncompress_Huffman_small_file :: Property
prop_compress_uncompress_Huffman_small_file = isJust (Huffman.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        input = "belle echelle !"
        (t, compressedData) = compress Huffman.tree input
        uncompressedData = uncompress (t, compressedData)

prop_compress_uncompress_Huffman_medium_file :: Property
prop_compress_uncompress_Huffman_medium_file = isJust (Huffman.tree (orderedCounts input)) ==>
  isJust t && isJust uncompressedData && input == fromJust uncompressedData
        where
        input = "Haskell est un langage de programmation fonctionnel fondé sur le lambda-calcul et la logique combinatoire.\nSon nom vient du mathématicien et logicien Haskell Curry. Il a été créé en 1990 par un comité de chercheurs en théorie des langages intéressés par les langages fonctionnels et l'évaluation paresseuse.\nLe dernier standard est Haskell 2010 : c'est une version minimale et portable du langage conçue à des fins pédagogiques et pratiques, dans un souci d'interopérabilité entre les implémentations du langage et comme base de futures extensions.\nLe langage continue d'évoluer en 2020, principalement avec GHC, constituant ainsi un standard de facto comprenant de nombreuses extensions."
        (t, compressedData) = compress Huffman.tree input
        uncompressedData = uncompress (t, compressedData)

return []
runTests :: IO Bool
runTests = $quickCheckAll

