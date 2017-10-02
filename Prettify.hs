-- file ch05/Prettify.hs

module Prettify where 

import Numeric
import Data.Bits

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
oneChar c = case lookup c simpleEscapes of 
  Just r -> text r
  Nothing | mustEscape c -> hexEscape c 
          | otherwise -> char c
  where 
    mustEscape c = c < '' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)] 
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where 
    ch a b = (a, ['\\',b])

smallHex:: Int -> Doc
smallHex n = text "\\u" <> text (replicate (4 - length h) 'o') <> text h
  where h = showHex n ""


astral:: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
  where d = ord c


fsep:: [Doc] -> [Doc]
fsep d = undefined

series:: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

punctuate:: Doc -> [Doc] -> [Doc]
punctuate [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p): punctuate p ds
