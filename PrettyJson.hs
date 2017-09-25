-- file: ch05/PrettyJson.hs

module PrettyJson where
import SimpleJson
import  Prettify

renderValue :: JValue -> Doc

renderValue (JBool True) = text "true"
renderValue (JBool False) = text "false"
renderValue JNull = text "null"
renderValue (JString s) = string s
renderValue (JNumber n) = double n
