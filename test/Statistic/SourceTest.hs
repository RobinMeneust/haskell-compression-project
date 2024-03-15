module SourceTest where

import Test.HUnit
import Statistic.Source

import Data.Map

-- Test cases for occurrences function
testOccurrences :: Test
testOccurrences = TestList [
    "Empty list" ~: occurrences ([] :: [Int]) ~?= emptyMap,
    "Non-empty list" ~: occurrences ([1, 2, 3, 1, 2] :: [Int]) ~?= expectedMap
  ]
  where
    emptyMap = (empty :: Data.Map.Map Int Int)
    expectedMap = fromList [(1, 2), (2, 2), (3, 1)]

-- Test cases for entropy function
testEntropy :: Test
testEntropy = TestList [
    "Empty list" ~: entropy ([] :: [Int]) ~?= 0.0,
    "Non-empty list" ~: entropy ['a', 'b', 'c', 'c', 'd', 'd', 'e', 'e', 'f', 'f', 'f', 'g', 'g', 'g', 'h', 'h', 'h', 'i', 'i', 'i'] ~?= expectedEntropy
  ]
  where
    expectedEntropy = 3.0709505944546693

-- Test cases for orderedCounts function
testOrderedCounts :: Test
testOrderedCounts = TestList [
    "Empty list" ~: orderedCounts ([] :: [Int]) ~?= [],
    "Non-empty list" ~: orderedCounts ([1, 1, 2, 2, 2, 3, 3, 3, 3] :: [Int]) ~?= [(3, 4), (2, 3), (1, 2)]
  ]

-- Combine all tests into one test suite
sourceTests :: Test
sourceTests = TestList
    [ testOccurrences
    , testEntropy
    , testOrderedCounts
    ]