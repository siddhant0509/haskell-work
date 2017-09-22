-- file Ch04/Plus.hs

a `plus` b = a + b

data a `Pair` b = a `Pair` b
  deriving (Show)


sf x =  let 
  a = 1 
  b = a
  in a + b + x

sf1 y = a + b
  where 
    a = 1
    b = a
