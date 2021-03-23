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
type Letters = ([Char],Char)

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
checkWordComposition cs w = null (w `minus` cs)

checkWordValidity :: Letters -> String -> Bool
checkWordValidity (ls,l) w = elem l w && checkWordComposition ls w

filterForGame :: Letters -> Dictionary -> Dictionary
filterForGame ls = filter (checkWordValidity ls)

score :: String -> Int
score word = (length word) - minWordLength + 1

maximumScore :: Dictionary -> Int
maximumScore = sum . map score

-- Determine the reason why a word was not valid - this somewhat replicates the code to filter the dictionary, but
-- returning a "reason" (String) instead of valid/invalid (Bool) 
invalidWordReason :: Letters -> [String] -> String -> String
invalidWordReason (ls,l) ws w 
    | length w < minWordLength        = "Played word must be minimum of " ++ (show minWordLength) ++ " letters."
    | not (elem l w)                  = "Played word must contain the letter \"" ++ (show l) ++ "\""
    | not (checkWordComposition ls w) = "You can only pick words composed with the letters \"" ++ ls ++ "\""
    | elem w ws                       = "You already guessed that word"
    | otherwise                       = "ERROR - cannot determine why \"" ++ w ++ "\" is not a valid word."

validatePlayedWord :: Dictionary -> Letters -> [String] -> String -> Either String Int
validatePlayedWord dict letters words word 
    | elem word dict && not (elem word words) = Right (score word)
    | otherwise                               = Left (invalidWordReason letters words word) 

-- Functions to pick letters for game, and to identify one letter as mandatory
randomLetters :: [Char] -> Int -> IO [Char]
randomLetters _ 0  = return []
randomLetters cs n = do
    i <- randomRIO (0, (length cs) - 1)
    let x = cs !! i
    xs <- randomLetters (filter (/= x) cs) (n-1)
    return (x : xs)

minus :: (Eq a) => [a] -> [a] -> [a]
minus [] _                 = []
minus (y:ys) xs | elem y xs = minus ys xs
                | otherwise = y : (minus ys xs)

lettersForGame :: IO Letters
lettersForGame = do
    (c:_) <- randomLetters ['a','e','i','o','u'] 1
    cs <- randomLetters (['a'..'z'] `minus` [c]) (lettersInGame - 1)
    let letters = c:cs
    (l:_) <- randomLetters letters 1
    return (letters,l)

-- Read a dictionary file, select 7 letters, use them to filter and score the dictionary
-- Could apply some rules to enhance this process, e.g. 
-- - Ensure that a 'u' is selected if a 'q' is selected
-- - Discard the letters and retry if max possible score is below some arbitrary value.
-- - Don't allow least frquently used letters to be mandatory (e.g. q, x, z?)
-- - Apply some weighting to selected letters - so more frequently used letters are more
--   likely to be selected (an inverse "Scrabble score" weighting)
dictionaryTest :: IO ()
dictionaryTest = do 
    fullDict <- readFile "english3.txt"
    letters <- lettersForGame
    let gameDict = filterForGame letters . filterDictionary . words $ fullDict
    print letters
    print gameDict
    print (maximumScore gameDict)