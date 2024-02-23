module Main (main) where

import LZ.LZ78
import Statistic.EncodingTree
import RLE

import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ show (RLE.compress "aaaabbcbbb")
    let encoded = RLE.compress "aaaabbcbbb" in putStrLn $ show (RLE.uncompress encoded)

    putStrLn $ show (LZ.LZ78.compress "belle echelle !")
    let encoded = LZ.LZ78.compress "belle echelle !" in putStrLn $ show (LZ.LZ78.uncompress encoded)

    -- let tree = EncodingNode 5 (EncodingNode 4 (EncodingLeaf 2 'a') (EncodingLeaf 2 'b')) (EncodingLeaf 1 'c') in putStrLn $ show (encode tree 'a')
