data JsonValue
  =   JsonString String
    | JsonNumber Double
    | JsonBool   Bool
    | JsonNull
    | JsonObject [(String, JsonValue)]
    | JsonArray  [JsonValue]
      deriving (Eq, Ord, Show)

stringFrom (JsonString wrapped)
  = Just wrapped
stringFrom _
  = Nothing

numberFrom (JsonNumber wrapped)
  = Just wrapped
numberFrom _
  = Nothing

boolFrom (JsonBool wrapped)
  = Just wrapped
boolFrom _
  = Nothing

objectFrom (JsonObject wrapped)
  = Just wrapped
objectFrom _
  = Nothing

arrayFrom (JsonArray wrapped)
  = Just wrapped
arrayFrom _
  = Nothing

isNull JsonNull
  = True
isNull _
  = False
