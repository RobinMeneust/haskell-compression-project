{-# LANGUAGE TemplateHaskell #-}
module RLESpec(runTests) where

import Test.QuickCheck

import RLE


return []
runTests :: IO Bool
runTests = $quickCheckAll

