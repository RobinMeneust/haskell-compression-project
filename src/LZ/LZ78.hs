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
uncompress _ = undefined -- TODO

compressRec :: String -> Dictionary -> String -> [(Int, Char)] -> [(Int, Char)]
compressRec "" _ _ acc = acc
compressRec text dict maxStr acc = 
    if isNothing index 
        then compressRec (tail text) ((newStr):dict) "" (acc ++ [(newIndex, firstChar)]) -- If it's not already in dict we can add it
        else compressRec (tail text) dict (maxStr ++ [firstChar]) acc -- If it's in the dict we add the char to the string to be searched in the dict
    where
        firstChar = head text
        index = findIndex (\x -> x == (maxStr ++ [firstChar])) dict
        newStr = maxStr ++ [firstChar]
        prevStepIndex = if maxStr == "" then Nothing else findIndex (\x -> x == maxStr) dict
        newIndex = if isNothing prevStepIndex then ((length dict) + 1) else fromJust prevStepIndex