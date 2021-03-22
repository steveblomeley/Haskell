type Dictionary = [String] 

dict :: Dictionary
dict = ["right","said","fred","have","a","cup","of","coffee","or","tea","disenfranchise","tautology"] 

minWordLength :: Int
minWordLength = 4

lettersInGame :: Int
lettersInGame = 7

-- Functions to refine a dictionary so that it's suitable for input into the game
-- - Remove words that are too short (standard rule is < 4 characters)
-- - Remove words that are too complex (standard rule is words that contain > 7  different characters)
filterShortWords :: Int -> Dictionary -> Dictionary
filterShortWords n = filter (\w -> (length w) >= n)

checkWordComplexity :: Int -> String -> Bool
checkWordComplexity _ [] = True
checkWordComplexity 0 _ = False
checkWordComplexity n (x:xs) = checkWordComplexity (n-1) (filter (/= x) xs)

filterComplexWords :: Int -> Dictionary -> Dictionary
filterComplexWords n = filter (checkWordComplexity n)

filterDictionary :: Dictionary -> Dictionary
filterDictionary = filterComplexWords lettersInGame . filterShortWords minWordLength

-- Functions to further refine dictionary to only contain words that are valid for a given set of game letters
-- i.e. words that must contain the first letter of the set, and are composed solely of letters from the set
checkWordComposition :: [Char] -> String -> Bool
checkWordComposition _ [] = True
checkWordComposition [] _ = False
checkWordComposition (c:cs) w = checkWordComposition cs (filter (/= c) w) 

checkWordValidity :: [Char] -> String -> Bool
checkWordValidity (c:cs) w = elem c w && checkWordComposition (c:cs) w

filterForGame :: [Char] -> Dictionary -> Dictionary
filterForGame cs = filter (checkWordValidity cs)

