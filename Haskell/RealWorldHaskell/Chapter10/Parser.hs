-- file: ch10/PNM.hs

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
    greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m bs) = 
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ ", of size " ++ show (L8.length bs) ++ " bytes"

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
  Nothing -> Nothing
  Just (num,rest)
    | num <= 0 -> Nothing
    | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

-- Now for the death march ...                    
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case getBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case getBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) -> 
                            Just (Greymap width height maxGrey bitmap, s6)

-- Two patterns in the death march:
-- - Functions with signature ... -> ByteString -> Maybe a b
-- - Case expressions that deconstruct a Maybe value, and either return 
--   Nothing or proceed to next step

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>? _ = Nothing
(Just x) >>? f = f x

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

parseP5' :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5' s =
  matchHeader (L8.pack "P5") s     >>?   -- (1) take bytes minus header
  \s -> skipSpace ((),s)           >>?   -- (2) coerce result of (1) to tuple so can strip whitespace from head of bytes
  (getNat . snd)                   >>?   -- (3) read natural from head of snd part of output from (2)
  skipSpace                        >>?   -- (4) strip whitespace from head of snd part of output from (3)
  \(width, s) -> getNat s          >>?   -- (5) name the output from (4), then read number from head of snd part
  skipSpace                        >>?   -- (6) strip whitespace from head of snd part of output from (5)
  \(height, s) -> getNat s         >>?   -- (7) name the output from (6), then read number from head of snd part
  \(maxGrey, s) -> getBytes 1 s    >>?   -- (8) name the output from (7), then discard 1 byte from head of snd part
  (getBytes (width*height) . snd)  >>?   -- (9) read image bytes from head of remaining bytes
  \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
