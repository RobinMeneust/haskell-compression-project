{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : Robin Meneust, Mathis TEMPO
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress, getOutputLength) where

import Statistic.Bit

import Data.Maybe

-- | Encoding tree used by Huffman and Shannon Fano methods
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
has :: Eq a => EncodingTree a -> a -> Bool
(EncodingLeaf _ a) `has` b = a == b
(EncodingNode _ left right) `has` b = left `has` b || right `has` b


-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode (EncodingLeaf _ b) c
    | b == c = Just [Zero]
    | otherwise = Nothing 
encode tree symb
    | tree `has` symb == False = Nothing -- If the symbol is not in the tree it's an error
    | otherwise = encodeRec tree symb []

-- | Computes the binary code of symbol using encoding tree with an accumulator
encodeRec :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
encodeRec (EncodingLeaf _ b) symb acc = if b == symb then Just acc else Nothing 
encodeRec (EncodingNode _ left right) symb acc
    | left `has` symb = encodeRec left symb (acc++[Zero]) 
    | otherwise = encodeRec right symb (acc++[One]) 

-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ b) bits = Just (b, bits)
decodeOnce (EncodingNode _ _ _) [] = Nothing 
decodeOnce (EncodingNode _ left _) (Zero:bits) = decodeOnce left bits 
decodeOnce (EncodingNode _ _ right) (One:bits) = decodeOnce right bits 

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeRec tree bits []

-- | Computes list of symbols from list of bits using encoding tree with a accumulator
decodeRec :: EncodingTree a -> [Bit] -> [a] -> Maybe [a]
decodeRec _ [] acc = Just acc 
decodeRec (EncodingLeaf c d) (b:bits) acc
    | b == Zero = decodeRec (EncodingLeaf c d) bits (d:acc)
    | otherwise = Nothing

decodeRec tree bits acc
    | isNothing temp = Nothing
    | otherwise = decodeRec tree nextBits nextAcc
    where
        temp = decodeOnce tree bits 
        nextBits = if isNothing temp then [] else snd (fromJust temp) 
        nextAcc = if isNothing temp then acc else acc++[fst (fromJust temp)] 
    

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = totalLength tree / fromIntegral (count tree)
  where
    totalLength (EncodingLeaf weight _) = fromIntegral weight
    totalLength (EncodingNode _ left right) = totalLength left + totalLength right

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress buildTree symbols =
  case buildTree symbols of
    Just tree -> let encodedSymbols = catMaybes $ map (encode tree) symbols
                 in (Just tree, concat encodedSymbols)
    Nothing -> (Nothing, [])


-- | Uncompress method using previously generated encoding tree
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress (Just tree, bits) = decode tree bits
uncompress (Nothing, _) = Nothing


-- | Get the size of the tree
getSizeTree :: (EncodingTree a) -> Int
getSizeTree (EncodingLeaf _ _) = 1
getSizeTree (EncodingNode _ left right) = (getSizeTree left) + (getSizeTree right) + 1

-- | Get size of the compressed data in bytes
getOutputLength :: (Maybe (EncodingTree a), [Bit]) -> Int
getOutputLength (Nothing, compressedData) = ((length compressedData) `div` 8) + (if length compressedData `mod` 8 == 0 then 0 else 1)
getOutputLength (Just tree, compressedData) = ((length compressedData) `div` 8) + (if length compressedData `mod` 8 == 0 then 0 else 1) + (getSizeTree tree)
