--file ch07 | toupper-imp.hs

import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do 
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  content <- hGetContents inH
  hPutStr outH (map toUpper content)
  hClose inH
  hClose outH


anotherMain :: IO ()
anotherMain = do
  content <- readFile "input.txt"
  writeFile "output.txt" (map toUpper content)

toUpperString :: String -> String
toUpperString = map toUpper

mainLoop :: Handle -> Handle -> IO ()
mainLoop inH outH = 
  do ineof <- hIsEOF inH
     if ineof
       then return ()
     else do 
       inpStr <- hGetLine inH
       hPutStrLn outH (map toUpper inpStr)
       mainLoop inH outH
       
