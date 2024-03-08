{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Jérémy SAELEN
-}
module LZ.LZW where

import LZ.Dictionaries
import Data.Maybe
import Data.List

-- | LZW compress method
compress :: String -> [Int]
compress text = compressRec text ascii "" []

compressRec :: String -> Dictionary -> String -> [Int] -> [Int]
compressRec "" dict maxStr acc
  | maxStr == "" = acc
  | otherwise = (acc ++ [fromJust previousIndex])
  where previousIndex = findIndex (\x -> x==maxStr) dict
compressRec text dict maxStr acc
  |isNothing index = compressRec text (dict ++ [newStr]) "" (acc ++ [fromJust previousIndex])
  |otherwise = compressRec (tail text) dict newStr acc
  where
  newStr = maxStr ++ [head text]
  index = findIndex (\x -> x==newStr) dict
  previousIndex = findIndex (\x -> x==maxStr) dict



-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress encoded = uncompressRec encoded ascii (Just "")


uncompressRec :: [Int] -> Dictionary -> Maybe String -> Maybe String
uncompressRec [] _ acc = acc

uncompressRec encoded dict acc
  | isNothing acc || length dict <= value = Nothing
  | otherwise = uncompressRec (tail encoded) newDict (Just (fromJust acc ++ character))
  where
    value = head encoded
    character = dict !! value
    newDict = if length (fromJust acc) == 0 then dict else dict ++ [([lastCharacter acc]++[(head character)])]

lastCharacter :: Maybe String -> Char
lastCharacter str = (fromJust str) !! (length (fromJust str) - 1)