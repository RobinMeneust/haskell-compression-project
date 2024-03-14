{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : Robin Meneust, (Add the other authors name here)
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
-- If computation is not possible, returns `Nothing`.
encodeRec :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
encodeRec (EncodingLeaf _ a) symb acc = if a == symb then Just acc else Nothing -- If we reached a leaf that corresponds to the symbol then we foudn the bits sequence and we return it, otherwise it's an error (it should not happen here)
encodeRec (EncodingNode _ left right) symb acc
	| left `has` symb = encodeRec left symb (acc++[Zero]) -- If the left tree has the symbol then we go left and we add a 0 to the bits sequence
	| otherwise = encodeRec right symb (acc++[One]) -- Otherwise it means that it's either in the right tree or not in the tree. So we go right and we add a 1. If it's not in the right tree this function will return Nothing (look at the case with the EncodingLeaf)


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ a) bits = Just (a, bits) -- It's a leaf so we found the symbol associated to the bits sequence, we can return it with the remaining bits sequence
decodeOnce (EncodingNode _ left right) [] = Nothing -- We don't have any bits left to be read and we aren't in a leaf, it's an error
decodeOnce (EncodingNode _ left right) (Zero:bits) = decodeOnce left bits -- We read a 0 so we go to the left node and we remove the bit read
decodeOnce (EncodingNode _ left right) (One:bits) = decodeOnce right bits -- We read a 1 so we go to the right node and we remove the bit read
decodeOnce _ _ = Nothing -- Error

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeRec tree bits []

-- | Computes list of symbols from list of bits using encoding tree with a accumulator
decodeRec :: EncodingTree a -> [Bit] -> [a] -> Maybe [a]
decodeRec tree [] acc = Just acc -- There are no remaining bit to be decoded
decodeRec tree bits acc
	| isNothing temp = Nothing -- If there is an error
	| otherwise = decodeRec tree nextBits nextAcc -- There are still bits to be decoded
	where
		temp = decodeOnce tree bits -- temp is the symbol that we got from the sequence of bits
		nextBits = if isNothing temp then [] else snd (fromJust temp) -- If there was an error nextBits is an empty list otherwise it's the remaining sequence of bits (we removed the bits read to decode the current symbol)
		nextAcc = if isNothing temp then acc else acc++[fst (fromJust temp)] -- We add to the accumulator the new symbol. If there was an error, we keep the accumulator as it is
	

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength _ = undefined -- TODO

-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress _ _ = undefined -- TODO

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress _ = undefined -- TODO
