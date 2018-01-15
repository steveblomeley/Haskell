-- Chapter 5 - Exercise 10 - Adapt crack function to work for mixed case (not just lowercase)

import Data.Char       -- for "chr" and "ord"
import Data.List       -- for "minimumBy"
import Data.Function   -- for "on"


-- List the letters in a string

letters :: String -> String
letters xs = [x | x <- xs, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')]


-- Count occurrences of char 'c' in a string

countChar :: Char -> String -> Int
countChar c xs = length [x | x <- xs, x == c]


-- Character to int, and back

charToInt :: Char -> Char -> Int
charToInt base c = ord c - ord base

intToChar :: Char -> Int -> Char
intToChar base n = chr(n + (ord base))

lCharToInt :: Char -> Int
lCharToInt c = charToInt 'a' c

uCharToInt :: Char -> Int
uCharToInt c = charToInt 'A' c

intToLChar :: Int -> Char
intToLChar n = intToChar 'a' n

intToUChar :: Int -> Char
intToUChar n = intToChar 'A' n


-- Apply shift 'n' to a letter

shiftChar :: Int -> Char -> Char
shiftChar n c
    | isLower c = intToLChar ((n + (lCharToInt c)) `mod` 26)
    | isUpper c = intToUChar ((n + (uCharToInt c)) `mod` 26)
    | otherwise = c


-- Apply shift 'n' to every letter in a string

encode :: Int -> [Char] -> [Char]
encode n xs = [shiftChar n x | x <- xs]


-- Calc percentage that one integer represents of another

percent :: Int -> Int -> Float
percent i n = (fromIntegral i / fromIntegral n) * 100.0


-- Calc frequency of occurrence of each letter 'a' (or 'A') -> 'z' (or 'Z') in a string

freqOfLetters :: [Char] -> [Float]
freqOfLetters xs = [percent ((countChar u xs) + (countChar l xs)) numLetters | (u,l) <- zip ['A'..'Z'] ['a'..'z']]
                  where numLetters = length(letters xs)


-- Calculate chi square statistic for observed VS expected frequencies

chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [ ((o - e) ^2) / e | (o,e) <- zip os es]


-- Relative frequencies of the letters a -> z in the english language

expectedFrequencies :: [Float]
expectedFrequencies = [8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.074]


-- left rotate a collection by N places

leftRotate :: Int -> [a] -> [a]
leftRotate n xs = (drop n xs) ++ (take n xs)


crack :: [Char] -> [Char]
crack xs = encode (-shift) xs
               where shift = fst(minimumBy (compare `on` snd) chiSquareResults)
                     chiSquareResults = [(n, chiSquare (leftRotate n observedFrequencies) expectedFrequencies) | n <- [0..25]]
                     observedFrequencies = freqOfLetters xs
