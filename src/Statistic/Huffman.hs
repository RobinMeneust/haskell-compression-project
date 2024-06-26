{- |
  Module : Statistic.Huffman
  Description : A module containing specifics for the Huffman compression method
  Maintainer : Mathis TEMPO
-}

module Statistic.Huffman (tree) where

import Statistic.EncodingTree (EncodingTree(..))
import Data.List (sortOn, sortBy)
import Data.Function (on)
import Statistic.Source(orderedCounts)

data HuffmanTree a = Leaf a Int | Node (HuffmanTree a) (HuffmanTree a) Int deriving (Show)

-- | generation of the huffman tree based on a list of frequencies
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing  -- Base case: Empty list
tree symbols = Just $ convertToEncodingTree $ buildTree $ (map (\(sym, freq) -> Leaf sym freq))  $ sortBy (flip compare `on` snd) $ orderedCounts symbols


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