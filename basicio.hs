-- file ch07 | basicio.hs

main = do
  putStrLn "Greeting! What is your name ?"
  inputS <- getLine
  putStrLn $ "Hi " ++ inputS
