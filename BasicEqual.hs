-- file: ch06| BasicEqual.hs


class BasicEqual a where
  isEqual :: a -> a -> Bool
  isEqual f s = not (isNotEqual f s)  

  isNotEqual :: a -> a -> Bool
  isNotEqual f s = not (isEqual f s)


instance BasicEqual Bool  where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False


data Color = Red | Green | Blue

instance BasicEqual Color where
  isEqual Red Red = True
  isEqual Green Green = True
  isEqual Blue Blue = True
  isEqual _ _ = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where 
  readsPrec _ val =
    tryParse [("Red", Red), ("Blue", Blue), ("Green", Green)]
      where 
        tryParse [] = []
        tryParse ((attempt, result):xs) = if (take (length attempt) (removeSpaces val)) == attempt then 
          [(result, drop (length attempt) val)] else tryParse xs

removeSpaces :: [Char] -> [Char]
removeSpaces [] = []
removeSpaces (x:xs) | x == ' ' = removeSpaces xs
                            | otherwise = x:xs 
         
