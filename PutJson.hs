module PutJson where

import Data.List (intercalate)
import JsonLibrary

renderJson :: JsonValue -> String
renderJson (JsonString value)
  = show value
renderJson (JsonNumber value)
  = show value
renderJson (JsonBool True)
  = "true"
renderJson (JsonBool False)
  = "false"
renderJson JsonNull
  = "null"

renderJson (JsonObject value)
  = "{" ++ pairs value ++ "}"
  where pairs []
          = ""
        pairs subject
          = foldl appendPair "" subject
        appendPair
          = appendPairSeparetedBy ", then"
        appendPairSeparetedBy separator result pair
          = result ++ pairPreceededBy separator pair
        pairPreceededBy pre pair
          = pre ++ valueOf pair
        valueOf (key, value)
          = renderJson value
