module Main
       (
         main
       ) where

import JsonLibrary
import PutJson

main
  = print(
  renderJson(JsonObject [("Foo", JsonNumber 1), ("Bar", JsonBool True)])
  )
