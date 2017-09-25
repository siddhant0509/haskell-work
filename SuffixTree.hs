-- file ch04 SuffixTree.hs

import Data.Char

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs:(suffixes xs')
suffixes [] = []

compose :: (a -> b) -> (b -> c) -> a ->  c
compose f g a = g (f a) 


camelCase :: String  -> String
camelCase line = (unwords . map (toUpperC) . words) line
  where 
    toUpperC (x:xs) = toUpper x : xs
