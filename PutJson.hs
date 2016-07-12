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
  where pairs []     = "" 
        pairs object = foldl renderPair "" object
        renderPair   = separate
        separate result (key, value) = result ++ ", then " ++ renderJson value
