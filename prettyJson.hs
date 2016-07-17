import JsonLibrary

data Doc
  = ToBeDefined
  deriving (Show)

string :: String -> Doc
string str
  = undefined

text :: String -> Doc
text str
  = undefined

double :: Double -> Doc
double num
  = undefined


renderJsonValue :: JsonValue -> Doc
renderJsonValue (JsonBool True)
  = text "true"

renderJsonValue (JsonBool False)
  = text "false"

renderJsonValue JsonNull
  = text "null"

renderJsonValue (JsonNumber num)
  = double num

renderJsonValue (JsonString str)
  = string str
