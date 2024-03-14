{- |
  Module      : Benchmark
  Description : Benchmark to compare the compression algoritms performances
  Maintainer  : Robin Meneust
-}
module Benchmark where

import System.IO -- To read files

import RLE
import LZ.LZ78 as LZ78
import LZ.LZW as LZW

-- | Compute and show LZ78 performance on the given input
test_LZ78 :: String -> IO ()
test_LZ78 input = do
    putStrLn $ "LZ78 Compression ratio: " ++ (show compressionRatio)
    putStrLn $ "LZ78 Space saving: " ++ (show spaceSaving) ++ " %"
    putStrLn ""
        where
            output = LZ78.compress input
            inputSize = length input
            outputSize = length output
            compressionRatio = (fromIntegral inputSize) / (fromIntegral outputSize)
            spaceSaving = 1.0 - ((fromIntegral outputSize) / (fromIntegral inputSize))

-- | Compute and show LZW performance on the given input
test_LZW :: String -> IO ()
test_LZW input = do
    putStrLn $ "LZW Compression ratio: " ++ (show compressionRatio)
    putStrLn $ "LZW Space saving: " ++ (show spaceSaving) ++ " %"
    putStrLn ""
        where
            output = LZW.compress input
            inputSize = length input
            outputSize = length output
            compressionRatio = (fromIntegral inputSize) / (fromIntegral outputSize)
            spaceSaving = 1.0 - ((fromIntegral outputSize) / (fromIntegral inputSize))

-- | Compute and show RLE performance on the given input
test_RLE :: String -> IO ()
test_RLE input = do
    putStrLn $ "RLE Compression ratio: " ++ (show compressionRatio)
    putStrLn $ "RLE Space saving: " ++ (show spaceSaving) ++ " %"
    putStrLn ""
        where
            output = RLE.compress input
            inputSize = length input
            outputSize = RLE.getOutputLength output
            compressionRatio = (fromIntegral inputSize) / (fromIntegral outputSize)
            spaceSaving = 1.0 - ((fromIntegral outputSize) / (fromIntegral inputSize))

benchmark :: IO ()
benchmark = do
    putStrLn "A good compression ratio is big and a good space saving is close to 1. If the space saving is negative then the compression is worst than the original file"
    putStrLn ""
    putStrLn "Starting tests..."
    fileHandle <- openFile "benchmark_input_files/repetitions.txt" ReadMode
    fileContent <- hGetContents fileHandle
    putStrLn "---- Test with a text with many repetitions ----"
    test_LZ78 fileContent
    test_LZW fileContent
    test_RLE fileContent
    
    putStrLn ""
    hClose fileHandle

    fileHandle <- openFile "benchmark_input_files/small.txt" ReadMode
    fileContent <- hGetContents fileHandle
    putStrLn "---- Test with a small text ----"
    test_LZ78 fileContent
    test_LZW fileContent
    test_RLE fileContent

    putStrLn ""
    hClose fileHandle


    fileHandle <- openFile "benchmark_input_files/medium.txt" ReadMode
    fileContent <- hGetContents fileHandle
    putStrLn "---- Test with a medium text ----"
    test_LZ78 fileContent
    test_LZW fileContent
    test_RLE fileContent
    
    putStrLn ""
    hClose fileHandle


    fileHandle <- openFile "benchmark_input_files/large.txt" ReadMode
    fileContent <- hGetContents fileHandle
    putStrLn "---- Test with a large text ----"
    test_LZ78 fileContent
    test_LZW fileContent
    test_RLE fileContent
    
    putStrLn ""
    hClose fileHandle


    fileHandle <- openBinaryFile "benchmark_input_files/image.png" ReadMode
    fileContent <- hGetContents fileHandle
    putStrLn "---- Test with an image ----"
    test_LZ78 fileContent
    test_LZW fileContent
    test_RLE fileContent
    
    putStrLn ""
    hClose fileHandle