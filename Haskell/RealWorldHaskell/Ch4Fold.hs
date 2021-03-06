import Data.Char

-- 1. Implement asInt using a fold
asInt :: String -> Int
asInt (x:xs) 
    | x == '-'  = - asInt' xs 
    | otherwise = asInt' (x:xs)
    where
        asInt' = foldl (\acc c -> (10*acc) + (digitToInt c)) 0

-- 5. Implement concat using foldr
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- 7. Recursive takeWhile vs using foldr
takeWhileRec :: (a -> Bool) -> [a] -> [a]
takeWhileRec _ [] = []
takeWhileRec p (x:xs)
    | p x       = x : (takeWhileRec p xs)
    | otherwise = []

takeWhileFoldr :: (a -> Bool) -> [a] -> [a]
takeWhileFoldr p = foldr (\x acc -> if p x then (x:acc) else []) []

-- 9. groupBy using a fold
groupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFold p = foldr grouper []
    where
        grouper x []     = [[x]]
        grouper x (y:ys) = if p x (head y) then (x:y) : (ys) else [x] : y : ys

-- 10. any, cycle, words and unlines - using folds?

-- any
anyFold :: (a -> Bool) -> [a] -> Bool
anyFold p = foldr (\x acc -> acc || p x) False

-- cycle [a,b,c] -> [a,b,c,a,b,c,a,b,c,a...ad nauseam]
--cycleFold :: [a] -> [a]
--cycleFold = ?

-- map
mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x acc -> (f x) : acc) []

-- words - not pretty! but does the job
wordsFold :: [Char] -> [[Char]]
wordsFold = snd . foldr helper (False,[[]])
    where 
        helper c (inspace,(x:xs))
            | isSpace c = (x /= [],  x:xs)
            | otherwise = (False, if inspace then [c]:x:xs else (c:x):xs)

-- unlines
unlinesFold :: [String] -> String
unlinesFold = foldr (\l ls -> if null ls then l else l ++ '\n' : ls) ""            