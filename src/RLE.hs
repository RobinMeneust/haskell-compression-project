{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : Robin Meneust
-}
module RLE(compress, uncompress) where

import Data.Maybe

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress input = compressRec input Nothing 0 []

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress input = uncompressRec input []

compressRec :: Eq a => [a] -> Maybe a -> Int -> [(a, Int)] -> [(a, Int)]
compressRec input prevSymb nbOcc acc
	| length input == 0 = if nbOcc>=1 then (acc ++ [((fromJust prevSymb),nbOcc)]) else acc -- End. If the last pair was not added we add it now
	| isNothing prevSymb && length input == 1 = [(currentSymb, 1)] -- first iteration and only one character
	| isNothing prevSymb = compressRec (tail input) (Just currentSymb) 1 acc -- first iteration
	| (fromJust prevSymb) == currentSymb = compressRec (tail input) (Just currentSymb) (nbOcc + 1) acc -- 2 characters are the same
	| otherwise = compressRec (tail input) (Just currentSymb) 1 (acc ++ [((fromJust prevSymb),nbOcc)]) -- 2 different characters
	where
		currentSymb = head input

uncompressRec :: [(a, Int)] -> [a] -> Maybe [a]
uncompressRec [] acc = Just acc
uncompressRec ((symb,occ):list) acc
	| occ>1 = uncompressRec ((symb,occ-1):list) (acc ++ [symb])
	| occ==1 = uncompressRec list (acc ++ [symb])
	| otherwise = Nothing