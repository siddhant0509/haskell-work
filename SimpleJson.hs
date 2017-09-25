-- file ch05 | SimpleJson.hs

module SimpleJson 
  (
    JValue(..)
    ,getString
    ,getInt
    ,getDouble
    ,getBool
    ,getObject
    ,getArray
    ,isNull
  ) where


data JValue = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
    deriving (Show, Eq, Ord)

getString :: JValue -> Maybe String
getString (JString s) =  Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber x) = Just (truncate x)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject x) = Just x
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray x) = Just x
getArray _ = Nothing

isNull :: JValue -> Bool
isNull (JNull) = True
isNull _ = False
