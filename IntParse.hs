-- file ch04/IntParse.hs

import Data.Char (digitToInt)


-- bad design as length is being calculated every time :'(
stringToInt:: String -> Int
stringToInt [] = error "Empty String"
stringToInt (x:[]) = digitToInt x
stringToInt (x:xs) = ((digitToInt x) * (10 ^ l)) + stringToInt xs
  where l = (length xs) 


-- 123 1*10 + 2*10 + 3

