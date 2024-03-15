module EncodingTreeTest where

import Test.HUnit
import Statistic.EncodingTree
import Statistic.Bit (Bit(..))

-- Test cases for isLeaf function
testIsLeaf :: Test
testIsLeaf = TestList
    [ TestCase $ assertBool "Leaf node" (isLeaf $ EncodingLeaf 0 'a')
    , TestCase $ assertBool "Non-leaf node" (not $ isLeaf $ EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b'))
    ]

-- Test cases for count function
testCount :: Test
testCount = TestList
    [ TestCase $ assertEqual "Leaf node count" 10 (count $ EncodingLeaf 10 'a')
    , TestCase $ assertEqual "Node count" 20 (count $ EncodingNode 20 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b'))
    ]

-- Test cases for has function
testHas :: Test
testHas = TestList
    [ TestCase $ assertBool "Symbol exists in leaf node" (has (EncodingLeaf 0 'a') 'a')
    , TestCase $ assertBool "Symbol exists in node" (has (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) 'b')
    , TestCase $ assertBool "Symbol does not exist" (not $ has (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) 'c')
    ]

-- Test cases for encode function
testEncode :: Test
testEncode = TestList
    [ TestCase $ assertEqual "Encoding symbol in leaf node" (Just [Zero]) (encode (EncodingLeaf 0 'a') 'a')
    , TestCase $ assertEqual "Encoding symbol in node" (Just [One]) (encode (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) 'b')
    , TestCase $ assertEqual "Symbol not found" Nothing (encode (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) 'c')
    ]

-- Test cases for decodeOnce function
testDecodeOnce :: Test
testDecodeOnce = TestList
    [ TestCase $ assertEqual "Decoding from leaf node" (Just ('a', [])) (decodeOnce (EncodingLeaf 0 'a') [])
    , TestCase $ assertEqual "Decoding from node" (Just ('b', [])) (decodeOnce (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) [One])
    , TestCase $ assertEqual "Invalid decoding" Nothing (decodeOnce (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) [Zero])
    ]

-- Test cases for decode function
testDecode :: Test
testDecode = TestList
    [ TestCase $ assertEqual "Decoding from leaf node" (Just "abc") (decode (EncodingLeaf 0 'a') [Zero, One, Zero])
    , TestCase $ assertEqual "Decoding from node" (Just "ab") (decode (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) [Zero, Zero])
    , TestCase $ assertEqual "Invalid decoding" Nothing (decode (EncodingNode 0 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b')) [Zero, One, One])
    ]

-- Test cases for meanLength function (Assuming a tree structure)
testMeanLength :: Test
testMeanLength = TestList
    [ TestCase $ assertEqual "Mean length with leaf node" 10.0 (meanLength $ EncodingLeaf 10 'a')
    , TestCase $ assertEqual "Mean length with node" 20.0 (meanLength $ EncodingNode 20 (EncodingLeaf 0 'a') (EncodingLeaf 0 'b'))
    ]

-- Test cases for compress function
testCompress :: Test
testCompress = TestList
    [ -- Test with a valid encoder function and input
      TestCase $ assertEqual "Compress with valid input"
          (Just (EncodingLeaf 0 'a'), [Zero, One, Zero])
          (compress (\_ -> Just (EncodingLeaf 0 'a')) "abc")

    -- Test with invalid encoder function (should return Nothing)
    , TestCase $ assertEqual "Compress with invalid input"
          (Nothing, [])
          (compress (\_ -> Nothing) "abc")
    ]

-- Test cases for uncompress function
testUncompress :: Test
testUncompress = TestList
    [ -- Test with a valid encoding tree and input
      TestCase $ assertEqual "Uncompress with valid input"
          (Just "abc")
          (uncompress (Just (EncodingLeaf 0 'a'), [Zero, One, Zero]))

    -- Test with invalid encoding tree (should return Nothing)
    , TestCase $ assertEqual "Uncompress with invalid encoding tree" (Nothing :: Maybe [Char]) 
          (uncompress (Nothing, [Zero, One, Zero]))
    ]




-- Combine all tests into one test suite
encodingTreeTests :: Test
encodingTreeTests = TestList
    [ testIsLeaf
    , testCount
    , testHas
    , testEncode
    , testDecodeOnce
    , testDecode
    , testMeanLength
    , testCompress
    , testUncompress
    ]