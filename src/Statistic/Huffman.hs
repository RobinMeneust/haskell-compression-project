{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : Mathis TEMPO
-}

module Statistic.Huffman (tree, calculateFrequencies) where

import Statistic.EncodingTree (EncodingTree(..))
import Data.List (sortOn, group, sort)

data HuffmanTree a = Leaf a Int | Node (HuffmanTree a) (HuffmanTree a) Int deriving (Show)

-- | generation of the huffman tree based on a list of frequencies
tree :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
tree [] = Nothing
tree xs = Just $ convertToEncodingTree $ buildTree $ map (\(sym, freq) -> Leaf sym freq) xs

-- | Building the tree
buildTree :: Ord a => [HuffmanTree a] -> HuffmanTree a
buildTree [] = error "buildTree called with an empty list"
buildTree [node] = node
buildTree nodes =
  let sortedNodes = sortOn frequency nodes
  in case sortedNodes of
       (node1:node2:rest) ->
         let newNode = Node node1 node2 (frequency node1 + frequency node2)
         in buildTree (newNode : rest)
       _ -> error "Unexpected pattern in buildTree"

-- | definition of the frequency
frequency :: HuffmanTree a -> Int
frequency (Leaf _ f) = f
frequency (Node _ _ f) = f

-- | Converting HuffmanTree in an EncodingTree
convertToEncodingTree :: HuffmanTree a -> EncodingTree a
convertToEncodingTree (Leaf sym freq) = EncodingLeaf freq sym
convertToEncodingTree (Node left right _) =
  EncodingNode (frequency left + frequency right) (convertToEncodingTree left) (convertToEncodingTree right)

-- | Calculation of the frequencies of the characters in the text
calculateFrequencies :: String -> [(Char, Int)]
calculateFrequencies = map (\l -> (head l, length l)) . group . sort