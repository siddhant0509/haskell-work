-- file ch06 | SimpleClass.hs
{-# TypeSynonymInstances, OverlappingInstances #-}

import Data.List

class Foo a where 
  foo :: a -> String
  
instance Foo a => Foo [a] where 
  foo = concat . intersperse "," . map foo

instance Foo Char where
  foo c = [c]

instance Foo Int where 
  foo = show

type S = String

ss :: String -> String
ss a = a
