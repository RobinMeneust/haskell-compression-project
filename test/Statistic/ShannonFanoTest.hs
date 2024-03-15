module ShannonFanoTest where

import Test.HUnit
import Statistic.ShannonFano
import Statistic.EncodingTree

-- Test Shannon-Fano tree generation
testShannonFanoTree :: Test
testShannonFanoTree = TestList
    [ TestCase $ assertEqual "Generates correct tree for an empty list"
        Nothing
        (tree ([] :: String))
    , TestCase $ assertEqual "Generates correct tree for a list with single symbol"
        (Just (EncodingLeaf 1 'a'))
        (tree "a")
    , TestCase $ assertEqual "Generates correct tree for a list of symbols"
        (Just (EncodingNode 6 (EncodingNode 4 (EncodingLeaf 2 'a') (EncodingLeaf 2 'b')) (EncodingLeaf 2 'c')))
        (tree "aabbcc")
    , TestCase $ assertEqual "Generates correct tree for a list with repeated symbols"
        (Just (EncodingLeaf 6 'a'))
        (tree "aaaaaa")
    ]

-- Combine all tests into one test suite
shannonFanoTests :: Test
shannonFanoTests = TestList
    [ testShannonFanoTree
    ]