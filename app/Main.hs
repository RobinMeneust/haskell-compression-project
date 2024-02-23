module Main (main) where

import LZ.LZ78

import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ show (compress "belle echelle !")
    let encoded = compress "belle echelle !" in putStrLn $ show (uncompress encoded)
