-- Chapter 5 - list comprehensions

import Data.Char       -- for "chr" and "ord"
import Data.List       -- for "minimumBy"
import Data.Function   -- for "on"


-- Test if a collection is sorted

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
 
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]


-- Lookup the value(s) corresponding to a given key in a list of K-V pairs

lookup :: Eq a => a -> [(a,b)] -> [b]
lookup k xs = [v | (k',v) <- xs, k == k']


-- Generate a list of the factors of "n"

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


-- Use factors to determine if "n" is prime? (yes, if factors of "n" are [1,n])

prime :: Int -> Bool
prime n = factors n == [1,n]
    

-- Use prime to generate list of primes between 1 and n

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]


-- List the lowercase letters in a string

lowers :: String -> String
lowers xs = [x | x <- xs, x >= 'a' && x <= 'z']


-- Count occurrences of char 'c' in a string

countChar :: Char -> String -> Int
countChar c xs = length [x | x <- xs, x == c]


-- Character to int, and back

charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

intToChar :: Int -> Char
intToChar n = chr(n + (ord 'a'))


-- Apply shift 'n' to a lowercase letter

shiftChar :: Int -> Char -> Char
shiftChar n c
    | isLower c = intToChar((n + (charToInt c)) `mod` 26)
    | otherwise = c


-- Apply shift 'n' to every lowercase letter in a string

encode :: Int -> [Char] -> [Char]
encode n xs = [shiftChar n x | x <- xs]


-- Calc percentage that one integer represents of another

percent :: Int -> Int -> Float
percent i n = (fromIntegral i / fromIntegral n) * 100.0


-- Calc frequency of occurrence of each lowercase letter 'a' -> 'z' in a string

freqOfLowers :: [Char] -> [Float]
freqOfLowers xs = [percent (countChar c xs) (length(lowers xs)) | c <- ['a'..'z']]


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
                     observedFrequencies = freqOfLowers xs

-- scalar product of 2 lists of integers

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y| (x,y) <- zip xs ys]