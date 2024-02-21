module Main (main) where

import LZ.LZ78 as LZ78
import LZ.LZW as LZW

import Data.List
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ show (LZ78.compress "belle echelle !")
    let encoded = LZ78.compress "belle echelle !" in putStrLn $ show (LZ78.uncompress encoded)
    putStrLn $ show (LZW.compress "belle echelle !")
    -- putStrLn $ show (findIndex (\x -> x == "le") ["b","e","l"])
    -- putStrLn $ show (firstChar)
    -- putStrLn $ show (newStr)
    -- putStrLn $ show (index)
    -- putStrLn $ show (prevStepIndex)
    -- putStrLn $ show (newIndex)
    -- where
    --     text = "e"
    --     maxStr = "l"
    --     dict = ["b","e","l"]
    --     firstChar = head text
    --     newStr = maxStr ++ [firstChar]
    --     index = findIndex (\x -> x == newStr) dict
    --     prevStepIndex = if maxStr == "" then Nothing else findIndex (\x -> x == maxStr) dict
    --     newIndex = if isNothing prevStepIndex then 0 else fromJust prevStepIndex
