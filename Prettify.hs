-- file ch05/Prettify.hs

module Prettify where 

data Doc = ToBeDefined
  deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double n = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right d = char left <> d <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar x = undefined	
