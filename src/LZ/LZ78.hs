{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : Robin Meneust
-}
module LZ.LZ78(compress, uncompress) where

import LZ.Dictionaries
import Data.Maybe
import Data.List

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress text = compressRec text empty "" []

-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress encoded = uncompressRec encoded empty (Just "")

-- | LZ78 compress method with accumulator
compressRec :: String 	-- ^ Text to be compressed
	-> Dictionary 		-- ^ Dictionary used for the compression
	-> String			-- ^ Current text section that is in the dictionary (if it's not then we add it to the dictionary and we reset this text section)
	-> [(Int, Char)]	-- ^ Accumulator, it corresponds to the compressed version of the text that has already been read
	-> [(Int, Char)]	-- ^ Compressed text
compressRec "" _ _ acc = acc
compressRec text dict maxStr acc
	| isNothing index = compressRec (tail text) (dict ++ [newStr]) "" (acc ++ [(newIndex, firstChar)]) -- If it's not already in dict we can add it
    | length text > 1 = compressRec (tail text) dict newStr acc -- If it's in the dict we add the char to the string to be searched in the dict
	| otherwise = (acc ++ [((if isNothing prevStepIndex then 0 else fromJust prevStepIndex), firstChar)]) -- If it's in the dict, but it's the last char, so we have to add it (e.g. "aa" becomes [(a,0),(a,0)] and "aaaaa" becomes [(a,0),(a,1),(a,1)])
    where
        firstChar = head text
        newStr = maxStr ++ [firstChar]
        index = findIndex (\x -> x == newStr) dict
        prevStepIndex = if maxStr == "" then Nothing else findIndex (\x -> x == maxStr) dict
        newIndex = if isNothing prevStepIndex then 0 else fromJust prevStepIndex

-- | LZ78 uncompress method with accumulator
-- If input cannot be uncompressed, returns `Nothing`
uncompressRec :: [(Int, Char)]	-- ^ Compressed data to be uncompressed
	-> Dictionary 				-- ^ Dictionary used to uncompress te data
	-> Maybe String				-- ^ Accumulator, it corresponds to the uncompressed version of the data that has already been read
	-> Maybe String				-- ^ Uncompressed data
uncompressRec [] _ acc = acc
uncompressRec encoded dict acc
    | isNothing acc = Nothing
	| firstVal < 0 = Nothing -- Invalid index
    | length dict <= firstVal = Nothing -- (n,x) but the nth entry is not found in the dict
    | firstVal /= 0 = uncompressRec (tail encoded) (dict ++ [(dict !! firstVal) ++ [firstChar]]) (Just ((fromJust acc) ++ (dict !! firstVal) ++ [firstChar])) -- New string
    | isNothing (findIndex (\x -> x == [firstChar]) dict) = uncompressRec (tail encoded) (dict ++ [[firstChar]]) (Just((fromJust acc) ++ [firstChar])) -- New char that is not already in the dict
    | otherwise = uncompressRec (tail encoded) dict (Just((fromJust acc) ++ [firstChar])) -- New char x: (0,x) but and x is already in dict (e.g. [(0,a),(0,a)] must give 'aa')
    where
        firstPair = head encoded
        firstVal = fst firstPair
        firstChar = snd firstPair

        