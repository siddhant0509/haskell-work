-- file ch08 | SumFile.hs

main = do 
  content <- getContents
  print (sumFile content)
  where 
    sumFile = sum . map read .  words


