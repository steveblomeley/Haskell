-- 1.

halve :: [a] -> ([a],[a])
halve xs =
    (take midpoint xs, drop midpoint xs)
    where midpoint = div (length xs) 2


-- 2.

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x


-- 3.

safeTail :: [a] -> [a]
safeTail xs = if (null xs) then [] else tail xs

safeTail' :: [a] -> [a]
safeTail' xs
    | null xs = []
    | otherwise = tail xs

safeTail'' :: [a] -> [a]
safeTail'' [] = []
safeTail'' xs = tail xs


-- 7.

multi :: (Num a) => a -> a -> a -> a
multi x = (\y -> (\z -> x * y * z))


-- 8.

luhnDouble :: (Integral a) => a -> a
luhnDouble x =
    if (doubled > 9) then doubled - 9 else doubled
    where doubled = 2 * x

luhn4 :: (Integral a) => a -> a -> a -> a -> Bool
luhn4 a b c d =
    if ((total `mod` 10) == 0) then True else False
    where total = (luhnDouble a) + b + (luhnDouble c) + d
