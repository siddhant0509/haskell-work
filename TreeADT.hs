-- file ch03/TreeADT.hs

data Tree a = Branch a (Tree a)  (Tree a)
    | Empty
    deriving (Show)

data TreeM a = TreeM a (Maybe (TreeM a)) (Maybe (TreeM a))
    deriving (Show)

second:: [a] -> a 
second (_:x:_) = x
second _ = error "Nope"

msecond:: [a] -> Maybe a
msecond (_:x:_) = Just x
msecond _ = Nothing

nodesAreSame (Branch a _ _) (Branch b _ _) | a == b = Just a
nodesAreSame _ _ = Nothing
