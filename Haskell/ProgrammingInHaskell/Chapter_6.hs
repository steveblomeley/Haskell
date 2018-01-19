-- Chapter 6 - Recursion

-- mutual recursion - eg 1

even' :: Int -> Bool
odd' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)
odd' 0 = False
odd' n = even' (n-1)

-- mutual recursion - eg 2

evens :: [a] -> [a]
odds :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs
odds [] = []
odds (_:xs) = evens xs

-- drop n elements from head of list
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop (n-1) xs

-- init (drop last element from non-empty list)
init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs

-- Ex 2. sum integers from n -> 1
sumdown :: Int -> Int
sumdown 1 = 1
sumdown n = n + sumdown (n-1)