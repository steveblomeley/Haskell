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
f1 :: Int -> Int
f1 = (\ x -> (x * 3))

f2 :: Int -> Int
f2 = (\x -> (x - 1))

fs :: [(Int -> Int)]
fs = [f1, f2]

f1thenf2 :: Int -> Int
f1thenf2 = foldr (.) id fs
