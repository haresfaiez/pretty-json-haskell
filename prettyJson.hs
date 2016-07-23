import JsonLibrary

data Doc
  = ToBeDefined
  deriving (Show)

<> :: Doc -> Doc -> Doc
<>
  = undefined

regular :: Char -> Doc
regular
  = undefined

concat :: [Doc] -> Doc
concat chunks
  = undefined

character :: Char -> Doc
character input
  = case lookup input escapeMap of
  Just espcaped        -> text    espcaped
  Nothing
    | mustEscape input -> escape  input
    | otherwise        -> regular input
    where mustEscape input
            =  input <  ' '
            || input == '\x7f'
            || input >  '\xff'

escapeMap :: [(Char, String)]
escapeMap
  = zipWith escapePair "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where escapePair subject identity
          = (subject, ['\\', identity])

hexadecimalCharacter :: Int -> Doc
hexadecimalCharacter input
  =  text "\\u"
  <> text (hexadecimal input)

hexadecimal :: Int -> String
hexadecimale value
    = hexadecimalOffset ++ (hexadecimalInset value)
hexadecimalOffset
    = replicate hexadecimalOffsetLength '0'
hexadecimalOffsetLength
    = 4 - length hexadecimalInset
hexadecimalInset value
    = showHex value ""

astral :: Int -> Doc
astral input
  =  hexadecimalCharacter (a + 0xd800)
  <> hexadecimalCharacter (b + 0xdc00)
  where a = (input `shiftR` 10) .&. 0x3ff
        b =  input              .&. 0x3ff

escape :: Char -> Doc
escape input
  | inputOrder < 0x10000 = hexadecimalCharacter d
  | otherwise            = astral (inputOrder - 0x10000)
  where inputOrder = ord input

enclose :: Char -> Char -> Doc -> Doc           
enclose opening closing fit
  =  character opening
  <> fit
  <> character closing

string :: String -> Doc
string
  = enclose '"' '"'
  . concat
  . map character

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

renderJsonValue (JsonString value)
  = string value
