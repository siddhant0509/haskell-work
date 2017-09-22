-- file ch03/Lending.hs

lend amount balance = let 
    reserve = 100
    newBalance = balance - amount
    in if balance < reserve
    then Nothing
    else Just newBalance
     

lend2::Int -> Int -> Maybe Int
lend2 amount balance = 
    if newBalance < reserve 
    then Nothing 
    else Just newBalance
    where 
      reserve = 100
      newBalance = balance - amount


lend3 amount balance 
    | amount <= 0 = Nothing
    | newbalance - reserve <= -1 = Nothing 
    | otherwise  = Just newbalance
    where 
      reserve = 100
      newbalance = balance - amount	
