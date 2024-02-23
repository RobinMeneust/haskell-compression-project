{-# LANGUAGE TemplateHaskell #-}
module LZ78Spec(runTests) where

import Test.QuickCheck

import LZ.LZ78

import Data.Maybe


prop_compress_empty :: Bool
prop_compress_empty = compress "" == []

prop_compress_uncompress :: String -> Bool
prop_compress_uncompress input =
    isJust output && input == fromJust output
    where
        output = uncompress (compress input)

prop_compress_uncompress_repetitions :: Char -> Property
prop_compress_uncompress_repetitions c =
	forAll (repetitions_str_gen c) $ \input -> let output = uncompress (compress input) in isJust output && input == fromJust output       

repetitions_str_gen :: Char -> Gen String
repetitions_str_gen c = listOf $ elements [c]


return []
runTests :: IO Bool
runTests = $quickCheckAll

