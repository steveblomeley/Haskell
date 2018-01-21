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

-- Ex 2. sum integers from n -> 0
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Ex 3. recursive exponentiation
(^*) :: Int -> Int -> Int
_ ^* 0 = 1
n ^* x = n * (n^*(x-1))

-- Ex 4. implement euclid's algorithm to determine greatest common divisor of 2 non -ve integers
-- Behaviour of empty list based on linq:
--           Enumerable.Empty<object>().All(x => true) == true
-- ...and... Enumerable.Empty<object>().All(x => false) == true
euclid :: Int -> Int -> Int
euclid x y
    | x == y = x
    | x > y  = euclid (x-y) y
    | x < y  = euclid x (y-x) -- meaning is clearer than "otherwise"

-- Ex 6. implementations of standard list functions 
all' :: [Bool] -> Bool
all' [] = True
all' (x:xs) = x && (all' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : (replicate' (n-1) x)

(!!!) :: [a] -> Int -> a
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys) = (x == y) || (elem' x ys)

-- Ex 7. merge 2 sorted lists into a single sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

-- Ex 8. use merge to implement a merge sort
--       v1. simplest approach, splits list of length N into N single element lists, then merges them
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge [x] (msort xs)

-- Ex 8. v2. halve the list, sort each half, then merge the sorted halves back together
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = (take n xs, drop n xs)
           where n = (length xs) `div` 2

msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' [x] = [x]
msort' xs = merge (msort' front) (msort' back)
            where (front,back) = halve xs

-- Ex 9. implementations of sum, take and last
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _  = []
take' n (x:xs) = x : (take (n-1) xs)

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs