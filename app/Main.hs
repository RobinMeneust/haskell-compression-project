module Main (main) where

import LZ.LZ78
import RLE

import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ show (RLE.compress "aaaabbcbbb")
    let encoded = RLE.compress "aaaabbcbbb" in putStrLn $ show (RLE.uncompress encoded)

    putStrLn $ show (LZ.LZ78.compress "belle echelle !")
    let encoded = LZ.LZ78.compress "belle echelle !" in putStrLn $ show (LZ.LZ78.uncompress encoded)
