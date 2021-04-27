module PrettyJSON where

import Numeric
import Data.Bits
import Data.Char

data Doc = ToBeDefined deriving (Show)

text :: String -> Doc
text s = undefined

char :: Char -> Doc
char c = undefined

-- append 2 docs - equivalent of ++
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- concat list of docs
dcat :: [Doc] -> Doc
dcat ds = undefined

-- map of common escape characters to their text representation
simpleEscapes :: [(Char,String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

-- functions to map unicode characters to a Doc - for both 4 digit and "astral" unicode characters    
smallHex :: Int -> Doc
smallHex n = text "\\u"
          <> text (replicate (4 - length h) '0')
          <> text h
    where h = showHex n ""

bigHex :: Int -> Doc
bigHex n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

-- can now map any hex character to corresponding Doc
hexEscape :: Char -> Doc
hexEscape c | n > 0x10000 = smallHex n
            | otherwise   = bigHex (n - 0x10000)
    where n = ord c

-- so we now have all we need to map any character ("normal" printable, "normal" escapable or "other" unicode) to corresponding Doc
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just se -> text se
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'   
    
-- now we can map any string to a doc by mapping individual characters and concatting the results
enclose :: Char -> Char -> Doc -> Doc
enclose left right d = char left <> d <> char right

string :: String -> Doc
string = enclose '"' '"' . dcat . map oneChar