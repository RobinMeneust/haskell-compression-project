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
	| tree `has` symb == False = Nothing
	| otherwise = encodeRec tree symb []

encodeRec :: Eq a => EncodingTree a -> a -> [Bit] -> Maybe [Bit]
encodeRec (EncodingLeaf _ a) symb acc = if a == symb then Just acc else Nothing
encodeRec (EncodingNode _ left right) symb acc
	| left `has` symb = encodeRec left symb (acc++[Zero])
	| otherwise = encodeRec right symb (acc++[One])


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ a) bits = Just (a, bits)
decodeOnce (EncodingNode _ left right) [] = Nothing
decodeOnce (EncodingNode _ left right) (Zero:bits) = decodeOnce left bits
decodeOnce (EncodingNode _ left right) (One:bits) = decodeOnce right bits
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode tree bits = decodeRec tree bits []

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
