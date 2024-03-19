{- |
  Module      : Benchmark
  Description : Benchmark to compare the compression algorithms performances
  Maintainer  : Robin Meneust
-}

module Benchmark(benchmark) where

import Statistic.EncodingTree as EncodingTree
import Statistic.Huffman as Huffman
import Statistic.ShannonFano as ShannonFano
import RLE
import LZ.LZ78 as LZ78
import LZ.LZW as LZW
import System.IO -- To read files

-- | Compute and show LZ78 performance on the given input
test_LZ78 :: String -> IO ()
test_LZ78 input = do
    putStrLn $ "LZ78 Compression ratio: " ++ show (compressionRatio :: Float)
    putStrLn $ "LZ78 Space saving: " ++ show (spaceSaving*100 :: Float) ++ " %"
    putStrLn ""
  where
    compressedData = LZ78.compress input
    inputSize = length input
    outputSize =  LZ78.getOutputLength compressedData
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - (fromIntegral outputSize / fromIntegral inputSize)

-- | Compute and show LZW performance on the given input
test_LZW :: String -> IO ()
test_LZW input = do
    putStrLn $ "LZW Compression ratio: " ++ show (compressionRatio :: Float)
    putStrLn $ "LZW Space saving: " ++ show (spaceSaving*100 :: Float) ++ " %"
    putStrLn ""
  where
    compressedData = LZW.compress input
    inputSize = length input
    outputSize = LZW.getOutputLength compressedData
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - (fromIntegral outputSize / fromIntegral inputSize)

-- | Compute and show RLE performance on the given input
test_RLE :: String -> IO ()
test_RLE input = do
    putStrLn $ "RLE Compression ratio: " ++ show (compressionRatio :: Float)
    putStrLn $ "RLE Space saving: " ++ show (spaceSaving*100 :: Float) ++ " %"
    putStrLn ""
  where
    compressedData = RLE.compress input
    inputSize = length input
    outputSize = RLE.getOutputLength compressedData
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - (fromIntegral outputSize / fromIntegral inputSize)

-- | Compute and show Shannon Fano performance on the given input
test_ShannonFano :: String -> IO ()
test_ShannonFano input = do
    putStrLn $ "Shannon Fano Compression ratio: " ++ show (compressionRatio :: Float)
    putStrLn $ "Shannon Fano Space saving: " ++ show (spaceSaving*100 :: Float) ++ " %"
    putStrLn ""
  where
    compressedData = EncodingTree.compress ShannonFano.tree input
    inputSize = length input
    outputSize = EncodingTree.getOutputLength compressedData
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - (fromIntegral outputSize / fromIntegral inputSize)

-- | Compute and show Huffman performance on the given input
test_Huffman :: String -> IO ()
test_Huffman input = do
    putStrLn $ "Huffman Compression ratio: " ++ show (compressionRatio :: Float)
    putStrLn $ "Huffman Space saving: " ++ show (spaceSaving*100 :: Float) ++ " %"
    putStrLn ""
  where
    compressedData = EncodingTree.compress Huffman.tree input
    inputSize = length input
    outputSize = EncodingTree.getOutputLength compressedData
    compressionRatio = fromIntegral inputSize / fromIntegral outputSize
    spaceSaving = 1.0 - (fromIntegral outputSize / fromIntegral inputSize)

-- | Test the compression algorithms on multiple files and compute the compression ratio to compare them
benchmark :: IO ()
benchmark = do
    putStrLn ""
    putStrLn "A good compression ratio is big and a good space saving is close to 1. If the space saving is negative then the compression is worse than the original file"
    putStrLn ""
    putStrLn "Starting tests..."
    testFiles ["repetitions.txt", "small.txt", "medium.txt", "large.txt", "image.png"]
  where
    testFiles [] = return ()
    testFiles (file:rest) = do
      putStrLn $ "---- Test with " ++ file ++ " (it might take some time for large files) ----"
      fileHandle <- openBinaryFile ("benchmark_input_files/" ++ file) ReadMode
      fileContent <- hGetContents fileHandle
      test_LZ78 fileContent
      test_LZW fileContent
      test_RLE fileContent
      test_ShannonFano fileContent
      test_Huffman fileContent
      putStrLn ""
      hClose fileHandle
      testFiles rest