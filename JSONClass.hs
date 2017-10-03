-- file ch06 | JsonClass.hs

{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import SimpleJson

type JsonError = String

class JSON a where 
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JsonError a

instance JSON JValue where 
  toJValue  = id
  fromJValue = Right

instance JSON Bool where 
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Not a json boolean"



doubleToJValue :: (Double -> a) -> JValue -> Either JsonError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "Number not a JSON number"

instance JSON Int where 
  toJValue = JNumber . realToFrac 
  fromJValue = doubleToJValue round

instance JSON Integer where 
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue  round

instance JSON Double where 
  toJValue = JNumber
  fromJValue = doubleToJValue id

instance (JSON a) => (JSON [a]) where 
  toJValue = undefined
  fromJValue = undefined



