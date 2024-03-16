module Statistic.Huffman (tree, calculateFrequencies) where

import Statistic.EncodingTree (EncodingTree(..))
import Data.List (sortOn, group, sort)

data HuffmanTree a = Leaf a Int | Node (HuffmanTree a) (HuffmanTree a) Int deriving (Show)

-- | Génération de l'arbre Huffman à partir d'une liste de fréquences
tree :: Ord a => [(a, Int)] -> Maybe (EncodingTree a)
tree [] = Nothing
tree xs = Just $ convertToEncodingTree $ buildTree $ map (\(sym, freq) -> Leaf sym freq) xs

-- | Construction de l'arbre Huffman
buildTree :: Ord a => [HuffmanTree a] -> HuffmanTree a
buildTree [node] = node
buildTree nodes =
  let sortedNodes = sortOn frequency nodes
      (node1:node2:rest) = sortedNodes
      newNode = Node node1 node2 (frequency node1 + frequency node2)
  in buildTree (newNode : rest)

-- | Insertion d'un noeud dans une liste de HuffmanTree, triée par fréquence
insertNode :: Ord a => HuffmanTree a -> [HuffmanTree a] -> [HuffmanTree a]
insertNode node [] = [node]
insertNode node (x:xs)
  | frequency node <= frequency x = node : x : xs
  | otherwise = x : insertNode node xs

-- | Récupération de la fréquence d'un HuffmanTree
frequency :: HuffmanTree a -> Int
frequency (Leaf _ f) = f
frequency (Node _ _ f) = f

-- | Conversion d'un HuffmanTree en EncodingTree
convertToEncodingTree :: HuffmanTree a -> EncodingTree a
convertToEncodingTree (Leaf sym freq) = EncodingLeaf freq sym
convertToEncodingTree (Node left right _) =
  EncodingNode (frequency left + frequency right) (convertToEncodingTree left) (convertToEncodingTree right)

-- | Calcul des fréquences des caractères dans une chaîne
calculateFrequencies :: String -> [(Char, Int)]
calculateFrequencies = map (\l -> (head l, length l)) . group . sort

