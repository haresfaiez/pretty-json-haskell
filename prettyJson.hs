import JsonLibrary

data Doc
  = ToBeDefined
  deriving (Show)

<> :: Doc -> Doc -> Doc
<>
  = undefined

asDoc :: Char -> Doc
asDoc
  = undefined

concatenateDoc :: [Doc] -> Doc
concatenateDoc documents
  = undefined

oneCharacter :: Char -> Doc
oneCharacter input
  = case lookup input escapes of
  Just replacement     -> text      replacement
  Nothing
    | mustEscape input -> hexEscape input
    | otherwise        -> asDoc     input
    where mustEscape input = input < ' ' || input == '\x7f' || input > '\xff'

escapes :: [(Char, String)]
escapes
  = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex value
  =  text "\\u"
  <> text (replicate (4 - length hex) '0')
  <> text hex
  where hex = showHex value ""

astral :: Int -> Doc
astral n
  = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise   = astral (d - 0x10000)
  where d = ord c

enclose :: Char -> Char -> Doc -> Doc           
enclose opening closing fit
  = asDoc opening <> fit <> asDoc closing

string :: String -> Doc
string
  = enclose '"' '"' . concatenateDoc . map oneCharacter

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
