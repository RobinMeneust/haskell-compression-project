{- |
  Module      : Benchmark
  Description : Benchmark to compare the compression algorithms performances
  Maintainer  : Robin Meneust
-}
module Benchmark where

import System.IO -- To read files
import qualified Statistic.EncodingTree as EncodingTree
import Statistic.Huffman as Huffman
import Statistic.ShannonFano as ShannonFano
import RLE
import LZ.LZ78 as LZ78
import LZ.LZW as LZW

-- | Compute and show LZ78 performance on the given input
test_LZ78 :: String -> IO ()
test_LZ78 input = do
    putStrLn $ "LZ78 Compression ratio: " ++ show compressionRatio
    putStrLn $ "LZ78 Space saving: " ++ show spaceSaving ++ " %"
    putStrLn ""
  where
    output = LZ78.compress input
    inputSize = length input
    outputSize = length output
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - fromIntegral outputSize / fromIntegral inputSize

-- | Compute and show LZW performance on the given input
test_LZW :: String -> IO ()
test_LZW input = do
    putStrLn $ "LZW Compression ratio: " ++ show compressionRatio
    putStrLn $ "LZW Space saving: " ++ show spaceSaving ++ " %"
    putStrLn ""
  where
    output = LZW.compress input
    inputSize = length input
    outputSize = length output
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - fromIntegral outputSize / fromIntegral inputSize

-- | Compute and show RLE performance on the given input
test_RLE :: String -> IO ()
test_RLE input = do
    putStrLn $ "RLE Compression ratio: " ++ show compressionRatio
    putStrLn $ "RLE Space saving: " ++ show spaceSaving ++ " %"
    putStrLn ""
  where
    output = RLE.compress input
    inputSize = length input
    outputSize = RLE.getOutputLength output
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - fromIntegral outputSize / fromIntegral inputSize

-- | Compute and show Shannon Fano performance on the given input
test_ShannonFano :: String -> IO ()
test_ShannonFano input = do
    putStrLn $ "Shannon Fano Compression ratio: " ++ show compressionRatio
    putStrLn $ "Shannon Fano Space saving: " ++ show spaceSaving ++ " %"
    putStrLn ""
  where
    output = EncodingTree.compress ShannonFano.tree input
    inputSize = length input
    outputSize = length output -- TODO: change to real size
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - fromIntegral outputSize / fromIntegral inputSize

-- | Compute and show Huffman performance on the given input
{- test_Huffman :: String -> IO ()
test_Huffman input = do
    let frequencies = calculateFrequencies input
        maybeTree = tree frequencies
        (maybeTreeAfterCompression, compressedBits) = EncodingTree.compress (const maybeTree) input
        outputSize = length compressedBits -- Assuming `compressedBits` is a list or similar
        inputSize = length input * 8 -- Multiply by 8 to convert char length to bit length if input is a string
        compressionRatio = fromIntegral inputSize / fromIntegral outputSize
        spaceSaving = 1.0 - fromIntegral outputSize / fromIntegral inputSize
    putStrLn $ "Huffman Compression ratio: " ++ show compressionRatio
    putStrLn $ "Huffman Space saving: " ++ show spaceSaving ++ " %"
    putStrLn "" -}

benchmark :: IO ()
benchmark = do
    putStrLn "A good compression ratio is big and a good space saving is close to 1. If the space saving is negative then the compression is worse than the original file"
    putStrLn ""
    putStrLn "Starting tests..."
    testFiles ["repetitions.txt", "small.txt", "medium.txt", "large.txt", "image.png"]
  where
    testFiles [] = return ()
    testFiles (file:rest) = do
      putStrLn $ "---- Test with " ++ file ++ " ----"
      fileContent <- readFile $ "benchmark_input_files/" ++ file
      test_LZ78 fileContent
      test_LZW fileContent
      test_RLE fileContent
      test_ShannonFano fileContent
      -- test_Huffman fileContent
      putStrLn ""
      testFiles rest