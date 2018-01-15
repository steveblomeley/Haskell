mySort :: (Ord a) => [a] -> [a]
mySort [] = []
mySort (x:xs) =
    let lowerRankedXs = mySort (filter (<x) xs)
        higherRankedXs = mySort (filter (>=x) xs)
    in lowerRankedXs ++ [x] ++ higherRankedXs


myFind :: (Eq a) => a -> [a] -> Maybe Int
myFind _ [] = Nothing
myFind t (x:xs)
    | (t == x) = Just 0
    | otherwise = case (myFind t xs) of
        Nothing -> Nothing
        Just n  -> Just (n + 1)
    
myFind' :: (Eq a) => a -> [a] -> Maybe Int
myFind' _ [] = Nothing
myFind' t (x:xs)
    | (t < x) = Nothing
    | (t == x) = Just 0
    | otherwise =
        let midpoint = div (length xs) 2
            front = take midpoint xs
            back = drop midpoint xs
        in if (head back) -- getting messy & procedural - come back to this another time.

