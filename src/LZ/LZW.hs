{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Jérémy SAELEN
-}
module LZ.LZW (compress, uncompress, removeSpecialCharacters) where

import LZ.Dictionaries
import Data.Maybe
import Data.List
import  Data.Char

-- | LZW compress method
compress :: String -> [Int]
compress text = compressRec (removeSpecialCharacters text "") ascii "" []

-- | Since LZW can't handle symbol with a "value" greater than 255 so we need to replace for instance \1000 with the cahracters \ 1 0 0 0
removeSpecialCharacters :: String -> String -> String
removeSpecialCharacters "" acc = acc
removeSpecialCharacters (c:text) acc
    | ord c > 255 = removeSpecialCharacters text (acc ++ [chr 92] ++ (show (ord c)))
    | otherwise = removeSpecialCharacters text (acc ++ [c])

-- | LZW compress method with accumulator
compressRec :: String   -- ^ Text to be compressed
    -> Dictionary       -- ^ Dictionary used for the compression
    -> String           -- ^ Current text section that is in the dictionary (if it's not then we add it to the dictionary and we reset this text section)
    -> [Int]            -- ^ Accumulator, it corresponds to the compressed version of the text that has already been read
    -> [Int]            -- ^ Compressed text
compressRec "" dict maxStr acc
    | maxStr == "" = acc -- It corresponds to the case where the original text is empty
    | otherwise = (acc ++ [fromJust previousIndex]) -- It corresponds to the case where it is the end of the text
    where 
        previousIndex = findIndex (\x -> x==maxStr) dict

compressRec text dict maxStr acc
	|isNothing index && isNothing temp = compressRec (tail text) (dict ++ [newStr]) "" (acc ++ [fromJust previousIndex]) -- If it's a character whose value is greater than 255 (not ASCII) and it's the first one
    |isNothing index = compressRec text (dict ++ [newStr]) "" (acc ++ [fromJust previousIndex]) -- If it's not already in dict we can add it
    |otherwise = compressRec (tail text) dict newStr acc -- If it's in the dict we add the char to the string to be searched in the dict
    where
        newStr = maxStr ++ [head text]
        index = findIndex (\x -> x==newStr) dict
        temp = findIndex (\x -> x==maxStr) dict
        previousIndex = if isNothing temp then Just (length ascii) else temp



-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress encoded = uncompressRec encoded ascii (Just "") 0


uncompressRec :: [Int]  -- ^ Compressed data to be uncompressed
    -> Dictionary       -- ^ Dictionary used to uncompress the data
    -> Maybe String     -- ^ Accumulator, it corresponds to the uncompressed version of the data that has already been read
    -> Int              -- ^ Previous value
    -> Maybe String     -- ^ Uncompressed data
uncompressRec [] _ acc _ = acc

uncompressRec (value:encoded) dict acc previousValue
    | isNothing acc || length dict < value || (length dict == value && length (fromJust acc) == 0) || value < 0 = Nothing -- If it cannot be uncompressed
    | otherwise = uncompressRec encoded newDict (Just (res ++ character)) value -- If it can be uncompressed
    where
        character = if value < (length dict) 
                then dict !! value
                else (dict !! previousValue) ++ (last [(dict !! previousValue)])
        res = fromJust acc
        lastCharacter = last res
        newDict = if length res == 0 
                then dict 
                else if value < (length dict) 
                    then dict ++ [([lastCharacter]++[(head character)])] 
                    else dict ++ [([lastCharacter]++[head character])] ++ [([lastCharacter]++(character))]

