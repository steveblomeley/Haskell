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
type Predicate a = a -> Bool
type Map a b = a -> b

filterAndMap :: Predicate a -> Map a b -> [a] -> [b]
filterAndMap p f xs = map f $ filter p xs


-- Ex 2. implement a version of each of the following functions from the prelude

all' :: Predicate a -> [a] -> Bool
all' p xs = foldr (\x a -> (p x) && a) True xs

any' :: Predicate a -> [a] -> Bool
any' p xs = foldr (\x a -> (p x) || a) False xs

takeWhile' :: Predicate a -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: Predicate a -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs

-- Ex 3. Redefine map f and filter p using foldr
map' :: Map a b -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

filter' :: Predicate a -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []


-- Ex 4. Convert list of N single digit ints into a single N digit int, e.g. [1,2,3] -> 123
intListToInt :: [Int] -> Int
intListToInt = foldl (\acc i -> 10 * acc + i) 0


-- Ex 5. Implement curry and uncurry
curry' :: ((a,b) -> c) -> a -> b -> c
--curry' f = (\x -> (\y -> f (x,y)))
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
--uncurry' f = (\(x,y) -> f x y)
uncurry' f = \(x,y) -> f x y


-- Ex 6. Using unfold - a recursive operation to generate a list from a single value

type Bit = Int

unfold :: Predicate a -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)

-- chop 8 (which I called stream2bytes)
stream2bytes' :: [Bit] -> [[Bit]]
stream2bytes' = unfold (== []) (take 8) (drop 8)

-- map f
map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f = unfold (== []) (f . head) tail

-- iterate f
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) f (\x -> x)


-- Ex 9. altMap - maps 2 functions to alternate members of a list
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs


-- Ex 10. use altMap to implement Luhn card check algorithm
luhnDouble :: (Integral a) => a -> a
luhnDouble x =
    if (doubled > 9) then doubled - 9 else doubled
    where doubled = 2 * x

luhn :: [Int] -> Bool
luhn xs = (sum luhned) `mod` 10 == 0
          where luhned = altMap (\x -> x) (\x -> luhnDouble x) (reverse xs)