--file: ch02/list.hs


myLength:: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNull:: [a] -> Bool
myNull [] = True
myNull _ = False

myHead:: [a] -> a
myHead [] = error "Empty List"
myHead (x:_) = x

myTail:: [a] -> [a]
myTail [] = error "Empty List"
myTail (_:xs) = xs

myLast:: [a] -> a
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myInit:: [a] -> [a]
myInit [] = []
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)


myAdd:: [a] -> [a] -> [a]
myAdd [] l2 = l2
myAdd (x:xs) l2 = x:(myAdd xs l2)

-- myConcat

myReverse:: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

myAnd:: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && (myAnd xs)

myOr:: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || (myOr xs)

myAll::  (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll p (x:xs) = p x && (myAll p xs)

myAny:: (a -> Bool) -> [a] -> Bool
myAny _ [] = True
myAny p (x:xs) = p x || (myAny p  xs)

myElem:: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem v (x:xs) = case (x == v) of 
  True -> True
  False -> myElem v xs


myFilter:: [a] -> (a -> Bool) -> [a]
myFilter [] _ = []
myFilter (x:xs) p = case (p x) of
  True -> x:(myFilter xs p)
  False -> myFilter xs p

myZip:: [a] -> [b] -> [(a,b)]
myZip [] l = []
myZip l [] = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myDrop n xs = if n <=0 || null xs
    then xs
    else myDrop (n-1) (tail xs)


lastButOne:: [a] -> a
lastButOne xs = if (length xs) == 2 
    then
        head xs
    else
        lastButOne (tail xs)



lastButOneRight [a,b] = a
lastButOneRight xs = lastButOneRight (tail xs)

