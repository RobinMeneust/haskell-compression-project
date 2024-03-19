{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : Robin Meneust
-}
module RLE(compress, uncompress, getOutputLength) where

import Data.Maybe
import Prelude

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress input = compressRec input Nothing 0 []

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress input = uncompressRec input []

-- | RLE compress method using an accumulator
compressRec :: Eq a => [a]  -- ^ Data to be compressed
    -> Maybe a              -- ^ Previous symbol read (Nothing if it's the first to be read)
    -> Int                  -- ^ Number of occurences of the previous symbol (0 if the previous symbol is Nothing (first iteration))
    -> [(a, Int)]           -- ^ Accumulator, it corresponds to the compressed version of the data that has already been read
    -> [(a, Int)]           -- ^ Compressed data
compressRec input prevSymb nbOcc acc
    | length input == 0 = if nbOcc>=1 then (acc ++ [((fromJust prevSymb),nbOcc)]) else acc -- End. If the last pair was not added we add it now
    | isNothing prevSymb && length input == 1 = [(currentSymb, 1)] -- first iteration and only one character
    | isNothing prevSymb = compressRec (tail input) (Just currentSymb) 1 acc -- first iteration
    | (fromJust prevSymb) == currentSymb = compressRec (tail input) (Just currentSymb) (nbOcc + 1) acc -- 2 characters are the same
    | otherwise = compressRec (tail input) (Just currentSymb) 1 (acc ++ [((fromJust prevSymb),nbOcc)]) -- 2 different characters
    where
        currentSymb = head input

-- | RLE uncompress method using an accumulator
-- If input cannot be uncompressed, returns `Nothing`
uncompressRec :: [(a, Int)] -> [a] -> Maybe [a]
uncompressRec [] acc = Just acc
uncompressRec ((symb,occ):list) acc
    | occ>1 = uncompressRec ((symb,occ-1):list) (acc ++ [symb]) -- We add the character to the output while its number of occurences is greater than 1
    | occ==1 = uncompressRec list (acc ++ [symb]) -- We only have one occurrence so we add the character only once to the output
    | otherwise = Nothing -- The number of occurences can't be null or negative

-- | Get size of the compressed data in bytes
getOutputLength :: [(a, Int)] -> Int
getOutputLength compressedData = getOutputLengthRec compressedData 0

-- | Get size of the compressed data in bytes with an accumulator
getOutputLengthRec :: [(a, Int)] -> Int -> Int
getOutputLengthRec [] acc = acc
getOutputLengthRec ((_,occ):list) acc = getOutputLengthRec list (acc + nbBytesForOcc + 1) -- size symbol + size occ in bytes
    where
        nbBytesForOcc = if occ > 0 then (div (floor (logBase 2.0 (fromIntegral occ) :: Float)) 8) + 1 else 1 -- e.g if occ = 1000 : log2(1000) / 8 + 1 = 2, we need 2 bytes to store this value