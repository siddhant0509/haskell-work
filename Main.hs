-- file: ch04/Main.hs

module Main(main) where

import SimpleJson

main = print (JObject [("foo", JNumber 1), ("bar", JNumber 2), ("baz", JString "YoYo")])
