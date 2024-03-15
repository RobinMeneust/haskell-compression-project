{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit
import Data.Maybe

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
-- has: Recursively checks if a given symbol exists in the encoding tree.
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ x) target = x == target
has (EncodingNode _ left right) target = has left target || has right target

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
-- encode: Finds the binary code of a symbol by traversing the encoding tree. If the symbol is found, it returns the corresponding binary code; otherwise, it returns Nothing.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ _) _ = Just [Zero]  -- Return [Zero] for leaf nodes
encode (EncodingNode _ left right) target
    | has left target = case encode left target of
                              Just bits -> Just (Zero : bits)
                              Nothing -> Nothing
    | has right target = case encode right target of
                               Just bits -> Just (One : bits)
                               Nothing -> Nothing
    | otherwise = Nothing


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
-- decodeOnce: Decodes the first symbol from a list of bits using the encoding tree and returns the remaining bits. If decoding is not possible, it returns Nothing.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ x) bits = Just (x, bits)
decodeOnce (EncodingNode _ left right) (bit:bits) =
    if bit == Zero
        then decodeOnce left bits
        else decodeOnce right bits
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
-- decode: Decodes the entire list of symbols from a list of bits using the encoding tree. If decoding is not possible at any point, it returns Nothing.
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decode' tree bits []
    where
        decode' _ [] acc = Just (reverse acc)
        decode' _ _ [] = Nothing
        decode' tree' bits' acc =
            case decodeOnce tree' bits' of
                Just (x, remainingBits) -> decode' tree remainingBits (x:acc)
                Nothing -> Nothing  -- If decoding fails, return Nothing


-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = fromIntegral totalLength / fromIntegral totalCount
  where
    (totalCount, totalLength) = traverseTree tree 0 0

    traverseTree :: EncodingTree a -> Int -> Int -> (Int, Int)
    traverseTree (EncodingLeaf cnt _) totalCnt totalLen = (totalCnt + cnt, totalLen + cnt)
    traverseTree (EncodingNode cnt left right) totalCnt totalLen =
      let (leftCnt, leftLen) = traverseTree left totalCnt totalLen
          (rightCnt, rightLen) = traverseTree right totalCnt totalLen
      in (totalCnt + cnt, totalLen + leftCnt + rightCnt)

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
-- compress: Compresses a list of symbols using a provided encoder function that generates an encoding tree. It returns the generated encoding tree along with the compressed binary data.
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress encoder input = case encoder input of
                            Just tree -> let encoded = mapM (encode tree) input
                                         in case encoded of
                                              Just bits -> (Just tree, concat bits)
                                              Nothing -> (Nothing, [])
                            _ -> (Nothing, [])

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
-- uncompress: Uncompresses binary data using a provided encoding tree. If uncompression is not possible, it returns Nothing.
uncompress :: Eq a => (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Just tree, bits) = decode tree bits
uncompress _ = Nothing
