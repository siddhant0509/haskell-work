-- file ch06| SimpleResult.hs

import SimpleJson

result :: JValue
result = JObject[
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
    JObject [
      ("title", JString "Simon Peyton Jones"),
      ("snippet", JString "Tackling the awkward")
      ]
    ])
  ] 
