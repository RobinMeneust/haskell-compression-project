{-# LANGUAGE TemplateHaskell #-}
module Statistic.ShannonFanoSpec(runTests) where

import Test.QuickCheck

import Statistic.ShannonFano as ShannonFano
import Statistic.EncodingTree
import Data.Maybe

generate_positive_int :: Gen Int
generate_positive_int = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

prop_build_tree_empty_string :: Bool
prop_build_tree_empty_string = isNothing(tree "")


prop_build_tree_single_char_repeat :: Char -> Property
prop_build_tree_single_char_repeat c =
    forAll generate_positive_int (\occ -> tree (take occ (repeat c)) == Just (EncodingLeaf occ c))


return []
runTests :: IO Bool
runTests = $quickCheckAll