{-# LANGUAGE TemplateHaskell #-}
module LZ78Spec(runTests) where

import Test.QuickCheck

import LZ.LZ78

import Data.Maybe


compress_empty :: Bool
compress_empty = compress "" == []

prop_compress_uncompress :: String -> Bool
prop_compress_uncompress input =
    isJust output && input == fromJust output
    where
        output = uncompress (compress input)

return []
runTests :: IO Bool
runTests = $quickCheckAll

