
-- 1/99 questions: Find the last element of a list.
-- (Can't handle empty list)

myLast (x : xs)
    | (null xs) = x
    | otherwise = myLast xs

-- Functional composition version

myLast' :: [a] -> a
myLast' = head . reverse

-- Which is equivalent to...

myLast'' :: [a] -> a
myLast'' x = head (reverse x)


-- 2/99 questions: Find the last but one element of a list.
-- (Can't handle lists containing <2 elements)

lastButOne :: [a] -> a
lastButOne (x : xs)
    | ((length xs) == 1) = x
    | otherwise          = lastButOne xs


-- 3/99 questions: Find the K'th element of a list (where first element == 1)
-- (Can't handle lists containing <k elements)

elementAt :: Int -> [a] -> a
elementAt n (x : xs)
    | (n == 1) = x
    | otherwise = elementAt (n - 1) xs
     

-- Version with more error handling

elementAt' :: Int -> [a] -> a
elementAt' n [] = error "Empty collection"
elementAt' n (x : xs)
    | n < 1 = error "Position must be >= 1"
    | (n - 1) > (length xs) = error "Requested position is off the end of the collection"
    | n == 1 = x
    | otherwise = elementAt (n - 1) xs


-- 4/99 questions: Return length of a list

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs
