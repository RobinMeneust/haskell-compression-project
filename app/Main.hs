module Main (main) where

import Statistic.EncodingTree as EncodingTree
import RLE
import LZ.LZ78 as LZ78
import LZ.LZW as LZW
import Statistic.Huffman as Huffman
import Statistic.ShannonFano as ShannonFano
import Statistic.Source as Source

import Data.List
import Data.Maybe

import Benchmark

main :: IO ()
main = do
	-- putStrLn $ show (LZW.removeSpecialCharacters "\0\5000\123e@\\" "")
	-- putStrLn $ show (LZW.compress "\0\5000\123e@\\")
	-- putStrLn $ show (LZW.uncompress (LZW.compress "\0\5000\123e\a@\\"))
	putStrLn $ show (LZW.compress "aaaaaaaaaa")
	putStrLn $ show (LZW.uncompress (LZW.compress "aaaaaaaaaa"))
	putStrLn $ show (LZW.compress "abababab")
	putStrLn $ show (LZW.uncompress (LZW.compress "abababab"))
    -- benchmark
	-- putStrLn $ show (Source.entropy "abbca")
	-- .putStrLn $ show (Huffmann.tree "abbca")
	-- putStrLn $ show (EncodingTree.compress Huffmann.tree "abbca")
	-- putStrLn $ show (ShannonFano.tree "abbca")
	-- putStrLn $ show (EncodingTree.compress ShannonFano.tree "a")
    -- putStrLn $ show (RLE.compress "aaaabbcbbb")
    -- let encoded = RLE.compress "aaaabbcbbb" in putStrLn $ show (RLE.uncompress encoded)

    -- putStrLn $ show (LZ78.compress "belle echelle !")
    -- let encoded = LZ78.compress "belle echelle !" in putStrLn $ show (LZ78.uncompress encoded)

    -- let tree = EncodingNode 5 (EncodingNode 4 (EncodingLeaf 2 'a') (EncodingLeaf 2 'b')) (EncodingLeaf 1 'c') in putStrLn $ show (encode tree 'a')
    -- putStrLn $ show (LZW.compress "belle echelle")
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
