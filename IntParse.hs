-- file ch04/IntParse.hs

import Data.Char (digitToInt)
import Data.Char (toUpper)

-- bad design as length is being calculated every time :'(
stringToInt:: String -> Int
stringToInt [] = error "Empty String"
stringToInt (x:[]) = digitToInt x
stringToInt (x:xs) = ((digitToInt x) * (10 ^ l)) + stringToInt xs
  where l = (length xs) 


-- 123 1*10 + 2*10 + 3

stringToInt2:: String -> Int
stringToInt2 x = loop 0 x 


loop:: Int -> String -> Int
loop v [] = v
loop v (x:xs) = loop v' xs
  where v' = v*10 + digitToInt x



square:: [Int] -> [Int]
square [] = []
square (x:xs) = (x*x):(square xs)


upperCase:: String -> String
upperCase [] = []
upperCase (x:xs) =  toUpper x : (upperCase xs)

myMap:: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : (myMap f xs)

myFilter:: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs) | p x = x:(myFilter p xs)
  | otherwise = myFilter p xs
myFilter _ _ = []


mySum:: [Int] -> Int
mySum (x:xs) = x + (mySum xs)
mySum [] = 0

mySumT:: [Int] -> Int
mySumT l = sum' 0 l
  where 
  sum' n (x:xs) = sum' (n + x) xs
  sum' n [] = n

myFoldl' :: (a -> b -> b) -> b -> [a] -> b
myFoldl' f d (x:xs) = myFoldl' f (f x d) xs
myFoldl' _ d [] = d


myFoldR' :: (a -> b -> b) -> b -> [a] -> b
myFoldR' f zero (x:xs) = f x (myFoldR' f zero xs)
myFoldR' _ zero [] = zero



filterFold:: (a -> Bool) -> [a] -> [a] 
filterFold p x = foldr myf [] x 
  where 
  myf a xs = case (p a) of 
    True -> a:xs
    False -> xs


mapFoldR:: (a -> b) -> [a] -> [b]
mapFoldR f x = foldr fn [] x 
  where  fn a z = (f a):z


myIdentity:: [a] -> [a]
myIdentity l = foldr (:) [] l

myAppend:: [a] -> [a] -> [a]
myAppend l1 l2 = foldr (:) l2 l1
