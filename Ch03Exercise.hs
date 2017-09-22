-- file ch03/Exercise.hs

--Q1 Q2
listLength:: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- Q3
avgList:: [Int] -> Double
avgList [] = 0
avgList xs = ( fromIntegral (sumList xs)) / (fromIntegral (listLength xs))

sumList:: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + (sumList xs)


listreverse:: [a] -> [a]
listreverse [] = []
listreverse (x:xs) = (listreverse xs) ++ [x]

--Q4
palindrome:: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ (listreverse xs)

--Q5
isPalindrome:: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs 
    | even (length xs) = (take half xs) == (reverse (drop half xs)) 
    | otherwise = (take (half - 1) xs) == (reverse (drop (half) xs))
    where half = round(fromIntegral(length xs)/2)
