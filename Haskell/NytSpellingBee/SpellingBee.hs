-- Command line game based on the New York Times Spelling Bee
--
-- ============================================================================================================
-- NOTE!!! This program imports System.Random 
-- So dirty fix to run in GHCi is to run: "stack ghci --package random"
-- Need to investigate how to create a proper build file so that this process is done automatically
-- ============================================================================================================
--
-- Program selects 7 random letters (must include >0 and <4 vowels)
-- One letter is nominated to be mandatory, i.e. must be present in every word played
-- Could enhance this process, e.g. 
-- - Ensure that a 'u' is selected if a 'q' is selected
-- - Discard the letters and retry if max possible score is below some arbitrary value.
-- - DONE: Don't allow least frequently used letters to be mandatory (e.g. q, x, z?)
-- - DONE: Apply some weighting to selected letters - so more frequently used letters are more
--         likely to be selected (a "Scrabble tiles" weighting)
--
-- The player enters words - to be valid, a word must:
-- - Contain >4 characters
-- - Contain at least one occurence of the mandatory letter
-- - Be comprised entirely from the 7 selected letters
-- - Be present in a dictionary of standard english words (http://gwicks.net/dictionaries.htm is one source)
-- - Not be a word that the player has already played
--
-- Each valid word is scored - 1 point for 4 letter word, + 1 extra point for each extra letter
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

-- These are the weightings used in Scrabble - i.e. the numbers of each letter in a standard
-- Scrabble set
letterWeightings :: [Int]
letterWeightings = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]

weightedLetters :: [Char]
weightedLetters = weightBy letterWeightings ['a'..'z']

weightedVowels :: [Char]
weightedVowels = filter (\x -> elem x ['a','e','i','o','u']) weightedLetters

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

validWord :: Letters -> String -> Bool
validWord (ls,l) w = elem l w && checkWordComposition ls w

filterForGame :: Letters -> Dictionary -> Dictionary
filterForGame ls = filter (validWord ls)

score :: String -> Int
score word = (length word) - minWordLength + 1

totalScore :: [String] -> Int
totalScore = sum . map score

data WordResult = Quit | Valid Int | NotValid String

-- Determine the reason why a word was not valid - this somewhat replicates the code to filter the dictionary, but
-- returning a "reason" (String) instead of valid/invalid (Bool) 
invalidWordReason :: Letters -> Dictionary -> [String] -> String -> String
invalidWordReason (ls,l) d ws w 
    | length w < minWordLength        = "Too short! Words must be at least " ++ (show minWordLength) ++ " letters"
    | not (elem l w)                  = "All words must contain the letter \"" ++ (show l) ++ "\""
    | not (checkWordComposition ls w) = "Only words containing the letters \"" ++ ls ++ "\" are valid"
    | elem w ws                       = "You already guessed that word"
    | not (elem w d)                  = "That word is not in the dictionary"
    | otherwise                       = "ERROR - cannot determine why \"" ++ w ++ "\" is not a valid word."

validatePlayedWord :: Dictionary -> Letters -> [String] -> String -> WordResult
validatePlayedWord dict letters words word 
    | word == ":q"                            = Quit
    | elem word dict && not (elem word words) = Valid (score word)
    | otherwise                               = NotValid (invalidWordReason letters dict words word) 

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

weightBy :: [Int] -> [a] -> [a]
weightBy ws = concat . zipWith replicate ws

lettersForGame :: IO Letters
lettersForGame = do
    (c:_) <- randomLetters weightedVowels 1
    cs <- randomLetters (weightedLetters `minus` [c]) (lettersInGame - 1)
    let letters = c:cs
    (l:_) <- randomLetters (letters `minus` ['q','x','z']) 1
    return (letters,l)

-- Read a dictionary file, select 7 letters, use them to filter and score the dictionary
dictionaryTest :: IO ()
dictionaryTest = do 
    fullDict <- readFile "dictionary.txt"
    letters <- lettersForGame
    let gameDict = filterForGame letters . filterDictionary . words $ fullDict
    print letters
    print gameDict
    print (totalScore gameDict)

-- Play the game    
printStatus :: Letters -> Dictionary -> [String] -> IO ()
printStatus (ls,l) dict ws = do
    putStrLn ("Your letters: " ++ ls ++ " - all words must include letter " ++ (show l))
    putStrLn ("Your score: " ++ show (totalScore ws) ++ " (total available: " ++ show (totalScore dict) ++ ")")

play :: Letters -> Dictionary -> [String] -> IO ()
play letters dict words = do
    printStatus letters dict words
    putStr "\nEnter a word: "
    w <- getLine
    case validatePlayedWord dict letters words w of
        Valid wordScore -> do
            putStrLn ("Good word! Score = " ++ show (wordScore))
            play letters dict (w:words)
        NotValid reason -> do
            putStrLn reason
            play letters dict words
        Quit -> do
            putStrLn "Possible words:"
            print dict
            putStrLn "\nBye!"

spellingBeeGame :: IO ()
spellingBeeGame = do    
    fullDict <- readFile "dictionary.txt"
    letters <- lettersForGame
    let gameDict = filterForGame letters . filterDictionary . words $ fullDict
    play letters gameDict []