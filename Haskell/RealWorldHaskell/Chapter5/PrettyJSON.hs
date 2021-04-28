module PrettyJSON (renderJValue) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import SimpleJSON (JValue(..))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

text :: String -> Doc
text "" = Empty
text s  = Text s

char :: Char -> Doc
char c = Char c

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

-- append 2 docs - equivalent of ++
(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y = x `Concat` y

-- concat list of docs
doccat :: [Doc] -> Doc
doccat = foldr (<>) empty

-- merge a list of Docs into one, possibly wrapping lines
docmerge :: [Doc] -> Doc
docmerge = foldr (</>) empty

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
string = enclose '"' '"' . doccat . map oneChar

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []  = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : (punctuate p ds)

-- common function to print repeating element (i.e. array or object)
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series left right fElement = enclose left right . docmerge . punctuate(char ',') . map fElement

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JNull)       = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

renderJValue (JArray a) = series '[' ']' renderJValue a

renderJValue (JObject o) = series '{' '}' renderField o
    where renderField (name,val) = text name
                                <> text ": "
                                <> renderJValue val

-- "compact" rendering - non-human-readable over-the-wire format
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = 
              case d of 
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  a `Union` _  -> transform (a:ds)

-- "pretty" rendering - a human-readable format that attempts to fit within specified line-width
pretty :: Int -> Doc -> String
pretty width doc = best 0 [doc]
    where best col (d:ds) =
              case d of
                  Empty        -> best col ds
                  Char c       -> c : best (col+1) ds
                  Text s       -> s ++ best (col + length s) ds
                  Line         -> '\n' : best 0 ds
                  a `Concat` b -> best col (a:b:ds)
                  a `Union`  b -> nicest col (best col (a:ds))
                                             (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


{-

x = JObject [("this", JBool True),("that", JNumber 1234)]

JObject [("this",JBool True),("that",JNumber 1234.0)]


y = renderJValue x

         +--03.Char '{'
   +--02.Concat 
   |     +                             +--09.Text "this"
   |     |                       +--08.Concat
   |     |                       |     +--10.Text ": "
   |     |                 +--07.Concat 
   |     |                 |     +--11.Text "true"
   |     |           +--06.Concat  
   |     |           |     +--12.Char ','
   |     |     +--05.Concat  
   |     |     |     |     +--14.Char ' '
   |     |     |     +--13.Union 
   |     |     |           +--15.Line
   |     +--04.Concat  
   |           |                 +--19.Text "that"
   |           |           +--18.Concat
   |           |           |     +--20.Text ": "
   |           |     +--17.Concat
   |           |     |     +--21.Text "1234.0"
   |           +--16.Concat  
   |                 |     +--23.Char ' '
   |                 +--22.Union
   |                       +--24.Line
01.Concat 
   +--25..Char '}'


pretty 80 y

01.Concat                                                                   ""    
  02.Concat 25.Char                                                         ""
    03.Char 04.Concat 25.Char                                               ""
    04.Concat 25.Char                                                       "{"
      05.Concat 16.Concat 25.Char                                           "{"
        06.Concat 13.Union 16.Concat 25.Char                                "{"
          07.Concat 12.Char 13.Union 16.Concat 25.Char                      "{"
            08.Concat 11.Text 12.Char 13.Union 16.Concat 25.Char            "{"
              09.Text 10.Text 11.Text 12.Char 13.Union 16.Concat 25.Char    "{"
              10.Text 11.Text 12.Char 13.Union 16.Concat 25.Char            "{this"
            11.Text 12.Char 13.Union 16.Concat 25.Char                      "{this: "
          12.Char 13.Union 16.Concat 25.Char                                "{this: true"
        13.Union 16.Concat 25.Char                                          "{this: true,"              
      16.Concat 25.Char                                                     "{this: true,\n"              
        17.Concat 22.Union 25.Char                                          "{this: true,\n"
          18.Concat 21.Text 22.Union 25.Char                                "{this: true,\n"
            19.Text 20.Text 21.Text 22.Union 25.Char                        "{this: true,\n"
            20.Text 21.Text 22.Union 25.Char                                "{this: true,\nthat"              
          21.Text 22.Union 25.Char                                          "{this: true,\nthat: "              
        22.Union 25.Char                                                    "{this: true,\nthat: 1234.0"              
      25.Char                                                               "{this: true,\nthat: 1234.0\n"              
                                                                            "{this: true,\nthat: 1234.0\n}"      
-}         