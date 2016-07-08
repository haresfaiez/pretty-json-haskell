data JsonValue
  =   JsonString String
    | JsonNumber Double
    | JsonBool   Bool
    | JsonNull
    | JsonObject [(String, JsonValue)]
    | JsonArray  [JsonValue]
      deriving (Eq, Ord, Show)

stringFrom :: JsonValue -> Maybe String
stringFrom (JsonString wrapped)
  = Just wrapped
stringFrom _
  = Nothing
