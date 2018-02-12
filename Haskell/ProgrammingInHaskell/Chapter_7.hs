-- foldr : accumulator func -> seed value -> input list -> result value
-- simplified version for lists only, instead of foldable
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- foldl : (order of params to accumulator func is reversed)
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

-- Two example usages of "id"

-- (1) Not the most useful - used with filter, will remove all "Falses" from a list of booleans
filtr :: [Bool] -> [Bool]
filtr = filter id

-- (2) Used with a fold to compose a single function from a list of functions
--     Though the results of this are a bit baffling...
f1 :: Int -> Int
f1 = (\ x -> (x * 3))

f2 :: Int -> Int
f2 = (\x -> (x - 1))

fs :: [(Int -> Int)]
fs = [f1, f2]

f1thenf2 :: Int -> Int
f1thenf2 = foldr (.) id fs


-- Ex. 1. express the list comprehension [f x | x <- xs, p x] in terms of map & filter
filterAndMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterAndMap p f xs = map f $ filter p xs


-- Ex 2. implement a version of each of the following functions from the prelude

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldr (\x a -> (p x) && a) True xs

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (\x a -> (p x) || a) False xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- Ex 3. Redefine map f and filter p using foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

-- Ex 4. Convert list of N single digit ints into a single N digit int, e.g. [1,2,3] -> 123
intListToInt :: [Int] -> Int
intListToInt is = foldl (\acc i -> 10 * acc + i) 0 is
