-- file ch04/ListADT.hs

data List a = Cons a (List a) 
    | Nil
    deriving (Show)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

buildList Nil = []
buildList (Cons x xs) = x:(buildList xs)
