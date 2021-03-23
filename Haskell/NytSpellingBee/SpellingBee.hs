-- Command line game based on the New York Times Spelling Bee
--
-- Program selects 7 random letters (must include >0 and <4 vowels)
-- One letter is nominated to be mandatory, i.e. must be present in every word played
--
-- The player enters words - to be valid, a word must:
-- - Contain >4 characters
-- - Contain at least one occurence of the mandatory letter
-- - Be comprised entirely from the 7 selected letters
-- - Be present in a dictionary of standard english words (http://gwicks.net/dictionaries.htm is one source)
-- - Not be a word that the player has already played
--
-- Each valid word is scored - 1 point for 4 letter word, then 1 extra point for each extra letter
--
-- At start of game, program uses the 7 letters in play to filter its standard dictionary for valid words
-- It then calculates the maximum possible score (i.e. if a player guessed all of the possible valid words)
-- After the player enters a valid word, the players score is updated 
--
-- After each "play", whether entered word is valid or not, the game re-displays:
-- - The 7 letters in play (highlighting the mandatory letter)
-- - Number of valid words played so far
-- - Player's score - "Score: <a>/<p> points" <- <a> is actual points, <p> is possible points
-- - Message indicating if the last word played was valid
--   - "Good word! x points."
--   - "You already played that word."
--   - "Too short - word must be at least 4 letters"
--   - "Word must contain the letter <m>" <- <m> is the mandatory letter
--   - "Word can only contain the letters a, b, c, d, e, f & g" <- where abcdefg are the letters in play for this game
--
-- The game will include some special commands - probably GHCi style, with ":" prefix
-- :q(uit)        quit the game - you'll be shown a list of all possible words (nice to have - highlight the ones
--                you guessed)
-- :n(ew)         start new game (you'll still be shown list of all possible words before new game starts)
-- :l(ist)        list all words guessed so far in alphabetical order
-- :s(huffle)     shuffle & re-display the 7 letters in play
-- :? or ?h(elp)  list all of these commands
import System.Random

type Dictionary = [String] 

dict :: Dictionary
dict = ["right","said","fred","have","a","cup","of","coffee","or","tea","disenfranchise","tautology"] 

minWordLength :: Int
minWordLength = 4

lettersInGame :: Int
lettersInGame = 7

-- Functions to refine a dictionary so that it's suitable for input into the game
-- - Remove words that are too short (i.e.  < 4 characters)
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
-- And to score a single word, and calculate the maximum possible score for a game (based on the filtered dictionary)
checkWordComposition :: [Char] -> String -> Bool
checkWordComposition _ [] = True
checkWordComposition [] _ = False
checkWordComposition (c:cs) w = checkWordComposition cs (filter (/= c) w) 

checkWordValidity :: [Char] -> String -> Bool
checkWordValidity (c:cs) w = elem c w && checkWordComposition (c:cs) w

filterForGame :: [Char] -> Dictionary -> Dictionary
filterForGame cs = filter (checkWordValidity cs)

score :: String -> Int
score word = (length word) - minWordLength + 1

maximumScore :: Dictionary -> Int
maximumScore = sum . map score

-- Determine the reason why a word was not valid - this somewhat replicates the code to filter the dictionary, but
-- returning a "reason" (String) instead of valid/invalid (Bool) 
invalidWordReason :: [Char] -> [String] -> String -> String
invalidWordReason (c:cs) ws w 
    | length w < minWordLength            = "Played word must be minimum of " ++ (show minWordLength) ++ " letters."
    | not (elem c w)                      = "Played word must contain the letter \"" ++ (show c) ++ "\""
    | not (checkWordComposition (c:cs) w) = "You can only pick words composed with the letters \"" ++ (c:cs) ++ "\""
    | elem w ws                           = "You already guessed that word"
    | otherwise                           = "ERROR - cannot determine why \"" ++ w ++ "\" is not a valid word."

validatePlayedWord :: Dictionary -> [Char] -> [String] -> String -> Either String Int
validatePlayedWord dict cs words word 
    | elem word dict && not (elem word words) = Right (score word)
    | otherwise                               = Left (invalidWordReason cs words word) 

-- Test out reading a dictionary file, and filtering it for use in the game

dictionaryTest :: IO ()
dictionaryTest = do 
    contents <- readFile "dictionary.txt"
    let dict = filterForGame "dkogmew" . filterDictionary . words $ contents
    print dict
    print (maximumScore dict)

-- Pick 7 letters for a game
-- This needs refinement to ensure that at least one vowel is always picked
randomLetters :: [Char] -> Int -> IO [Char]
randomLetters _ 0  = return []
randomLetters cs n = do
    i <- randomRIO (0, (length cs) -1)
    let x = cs !! i
    xs <- randomLetters (filter (/= x) cs) (n-1)
    return (x : xs)