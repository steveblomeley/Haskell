import Data.Array (Array(..), listArray, (!), Ix, indices, bounds)
import Data.Char (digitToInt)

-- Barcode recognition example

checkDigit :: Integral a => [a] -> a
checkDigit as = if result == 0 then result else 10 - result
    where
        result = sum factoredDigits `mod` 10
        factoredDigits = mapAlternate (*3) (reverse as) 

mapAlternate :: (a -> a) -> [a] -> [a]
mapAlternate f = zipWith ($) (cycle [f,id])

-- Define 4 arrays of bit encodings:
-- - Two 7 bit encodings for "left digits" - one with odd parity; one with even
-- - A 7 bit encoding for "right digits"
-- - A 6 bit encoding for the initial digit, based on the parity bits of the six left digits

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList
    where
        complement '0' = '1'
        complement '1' = '0'            

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,len-1) xs
    where len = length xs

leftOddValues  = listToArray leftOddList
leftEvenValues = listToArray leftEvenList
rightValues    = listToArray rightList
parityValues   = listToArray parityList
terminalMarker = "101"
middleMarker   = "01010"

-- Define fold functions - no generic versions defined in libraries as there
-- are too many variations of dimensions and directions of traversal
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f acc xs = go acc (indices xs)
    where go acc []     = acc
          go acc (i:is) = let acc' = f acc (xs ! i)
                          in acc' `seq` go acc' is

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f xs = foldA f (xs ! i) xs
    where i = fst (bounds xs)                          

-- An EAN13 encode function, so that we understand the encoding, and can test
-- the decode function in due course
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- Encode digits - this encodes a list of 12 digits into a 13 digit EAN
-- The 13 digit EAN comprises:
--
-- = Start marker
-- - Digits 2 -> 7, encoded using left or right parity, such that the series 
--   of parity bits also encodes the first digit
-- = Middle marker
-- - Digits 8 -> 12, plus the check digit
-- = End marker  
encodeDigits :: [Int] -> [String]
encodeDigits all@(d:ds) = terminalMarker : leftDigits ++ middleMarker : rightDigits ++ [terminalMarker]
    where
        leftDigits  = leftEncode d left6
        rightDigits = rightEncode (right5 ++ [checkDigit all])
        (left6, right5) = splitAt 6 ds

leftEncode :: Int -> [Int] -> [String]
leftEncode p ds = [ result ]
    where result = "LHS = parity encode " ++ (show p) ++ " left encoded " ++ (show ds)

rightEncode :: [Int] -> [String]
rightEncode ds = [ result ]
    where result = "RHS = right encoded " ++ (show ds)