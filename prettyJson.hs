module PrettyJSON
       (
         renderJValue
       ) where

import Numeric
import Data.Char
import Data.Bits
import JsonLibrary
import Prettify

data Doc = Empty
  | Char   Char
  | Text   String
  | Line
  | Concat Doc Doc
  | Union  Doc Doc
   deriving (Show,Eq)

empty :: Doc
empty = Empty

line :: Doc
line = Line

regular :: Char -> Doc
regular character = Char character
  
<> :: Doc -> Doc -> Doc
<> Empty value = value
<> value Empty = value
<> left right  = left `Concat` right

concat :: [Doc] -> Doc
concat chunks = fold (<>)

wrapConcat :: [Doc] -> Doc
wrapConcat = fold (</>)

(</>) :: Doc -> Doc -> Doc
first </> last = first <> softline <> last

softline :: Doc
softline = flatUnion line

flatUnion :: Doc -> Doc
flatUnion input = flat input `Union` input

flat :: Doc -> Doc
flat (first `Concat` last) = flat first `Concat` flat last
flat Line                  = Char ' '
flat (singleton `Union` _) = flat singleton
flat aDoc                  = aDoc

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold join = foldr join empty

separate :: Doc -> [Doc] -> [Doc]
separate _ []                    = []
separate _ [singleton]           = [singleton]
separate separator (each:others) = (each <> separator) : separate separator others

character :: Char -> Doc
character input = case lookup input escapeMap of
  Just espcaped        -> text    espcaped
  Nothing
    | mustEscape input -> escape  input
    | otherwise        -> regular input
    where mustEscape input
            =  input <  ' '
            || input == '\x7f'
            || input >  '\xff'

escapeMap :: [(Char, String)]
escapeMap = zipWith escapePair "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where escapePair subject identity
          = (subject, ['\\', identity])

hexadecimalCharacter :: Int -> Doc
hexadecimalCharacter input =  text "\\u" <> text (hexadecimal input)

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
astral input =  hexadecimalCharacter (a + 0xd800) <> hexadecimalCharacter (b + 0xdc00)
  where a = (input `shiftR` 10) .&. 0x3ff
        b =  input              .&. 0x3ff

escape :: Char -> Doc
escape input
  | inputOrder < 0x10000 = hexadecimalCharacter d
  | otherwise            = astral (inputOrder - 0x10000)
  where inputOrder = ord input

enclose :: Char -> Char -> Doc -> Doc           
enclose open close fit =  character open <> fit <> character close

string :: String -> Doc
string = enclose '"' '"'
  . concat
  . map character

text :: String -> Doc
text "" = Empty
text input = Text input

double :: Double -> Doc
double value = Text (show value)

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close doc = enclose open close
  . wrapConcat
  . separate separator 
  . map      doc
  where separator = (character ',')

renderJsonValue :: JsonValue -> Doc
renderJsonValue (JsonBool True) = text "true"

renderJsonValue (JsonBool False) = text "false"

renderJsonValue JsonNull = text "null"

renderJsonValue (JsonNumber num) = double num

renderJsonValue (JsonString wrap) = string wrap

renderJsonValue (JArray wrap) = series '[' ']' renderJsonValue wrap

renderJsonValue (JObject wrapped) = series '{' '}' field wrapped
  where field (key, value)
          =  string          key
          <> text            ": "
          <> renderJsonValue value
 
