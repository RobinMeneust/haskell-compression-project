{-# LANGUAGE TemplateHaskell #-}
module Statistic.EncodingTreeSpec(runTests) where

import Test.QuickCheck

import Statistic.EncodingTree
import Statistic.Bit

encodingTree = EncodingNode 5 (EncodingNode 4 (EncodingLeaf 2 'a') (EncodingLeaf 2 'b')) (EncodingLeaf 1 'c')

-- Those tests are incomplete and should be re-written

prop_has :: Char -> Bool
prop_has c = encodingTree `has` c == elem c ['a','b','c']



prop_encode_a :: Bool
prop_encode_a = encode encodingTree 'a' == Just [Zero,Zero]

prop_encode_b :: Bool
prop_encode_b = encode encodingTree 'b' == Just [Zero,One]

prop_encode_c :: Bool
prop_encode_c = encode encodingTree 'c' == Just [One] 



prop_decodeOnce_a :: Bool
prop_decodeOnce_a = decodeOnce encodingTree [Zero,Zero] == Just ('a',[])

prop_decodeOnce_b :: Bool
prop_decodeOnce_b = decodeOnce encodingTree [Zero,One] == Just ('b',[])

prop_decodeOnce_c :: Bool
prop_decodeOnce_c = decodeOnce encodingTree [One]  == Just ('c',[])

prop_decode :: Bool
prop_decode = decode encodingTree [Zero,Zero,Zero,One,One] == Just "abc"

return []
runTests :: IO Bool
runTests = $quickCheckAll

