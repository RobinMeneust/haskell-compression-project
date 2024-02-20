module Main (main) where

import LZ.LZ78

main :: IO ()
main = do
	putStrLn $ show (compress "belle echelle !")
