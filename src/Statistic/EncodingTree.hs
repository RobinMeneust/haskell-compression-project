{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : Robin Meneust, Mathis TEMPO
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress, calculateFrequencies) where

import Statistic.Bit

import Data.Maybe

import Data.List (group, sort)

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
(EncodingLeaf cnt a) `has` b = a == b
(EncodingNode cnt left right) `has` b = left `has` b || right `has` b


-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit]
encode tree symb
	| tree `has` symb == False = Nothing -- If the symbol is not in the tree it's an error
	| otherwise = encodeRec tree symb []

-- | Computes the binary code of symbol using encoding tree with an accumulator
encodeRec :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
encodeRec (EncodingLeaf _ a) symb acc = if a == symb then Just acc else Nothing 
encodeRec (EncodingNode _ left right) symb acc
	| left `has` symb = encodeRec left symb (acc++[Zero]) 
	| otherwise = encodeRec right symb (acc++[One]) 


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ a) bits = Just (a, bits)
decodeOnce (EncodingNode _ left right) [] = Nothing 
decodeOnce (EncodingNode _ left right) (Zero:bits) = decodeOnce left bits 
decodeOnce (EncodingNode _ left right) (One:bits) = decodeOnce right bits 
decodeOnce _ _ = Nothing -- Error

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeRec tree bits []

-- | Computes list of symbols from list of bits using encoding tree with a accumulator
decodeRec :: EncodingTree a -> [Bit] -> [a] -> Maybe [a]
decodeRec tree [] acc = Just acc 
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

-- | Calculation of the frequencies of the characters in the text
calculateFrequencies :: Ord a => [a] -> [(a, Int)]
calculateFrequencies = map (\l -> (head l, length l)) . group . sort