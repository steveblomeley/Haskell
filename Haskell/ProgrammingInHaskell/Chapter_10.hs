import System.IO
import Data.Char

-- Example functions based on IO primitives
getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                  return []
              else
                  do xs <- getLine
                     return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

getStrlen :: IO ()
getStrlen = do putStr' "Enter a string: "
               xs <- getLine'
               putStr' "You typed "
               putStr' xs
               putStr' " which has "
               putStr' (show (length xs))
               putStrLn' " characters."

-- Game of hangman, v1. Guess the entire word

getCharNoEcho :: IO Char
getCharNoEcho = do hSetEcho stdin False
                   x <- getChar
                   hSetEcho stdin True
                   return x

getLineNoEcho :: IO String
getLineNoEcho = do x <- getCharNoEcho
                   if x == '\n' then
                       do putChar x
                          return []
                   else
                       do putChar '*'
                          xs <- getLineNoEcho
                          return (x:xs)

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]                    

play :: String -> IO ()
play word = do putStr "Your guess? "
               guess <- getLine'
               if guess == word then
                   putStrLn' "You got it!"
               else
                   do putStrLn (match word guess)
                      play word

hangman :: IO ()
hangman = do putStrLn' "Type a word: "
             word <- getLineNoEcho
             putStrLn' "Try to guess the word..."
             play word                        

-- Hangman v2. Incrementally guess individual letters

-- Display all the letters that have been guessed so far
-- Assumes that all guesses are lowercase letters
showGuesses :: String -> String -> String
showGuesses [] template = template
showGuesses (x:xs) template = showGuesses xs guessesSoFar
                        where 
                            pos = ord x - ord 'a'
                            head = take pos template
                            tail = drop (pos+1) template
                            guessesSoFar = head ++ [x] ++ tail

-- Display the correct letters that have been guessed so far                           
showMatches :: String -> String -> String
showMatches _ []           = []
showMatches guesses (x:xs) | elem x guesses = x : matchRemaining
                           | otherwise      = '_' : matchRemaining      
                           where 
                               matchRemaining = showMatches guesses xs 

-- Check if every letter in the word has been guessed                               
matches :: String -> String -> Bool
matches _ [] = True
matches guesses (x:xs) | elem x guesses = matches guesses xs
                       | otherwise      = False                                

-- Check whether newly guessed letter has solved the puzzle - if not, display all guesses
-- and correct guesses so far, and continue                    
checkGuess :: Char -> String -> String -> IO ()
checkGuess g gs word | g == '\n' = play'' gs word
                     | otherwise = if matches (g:gs) word then
                                       do putStrLn' (word ++ " - You got it!")
                                   else
                                       do let template = take 26 (repeat '_')
                                          putStrLn ("Guesses: " ++ (showGuesses (g:gs)) template)
                                          putStrLn ("Matches: " ++ (showMatches (g:gs) word))
                                          putStrLn "Guess another letter: "
                                          play'' (g:gs) word

-- Get next guess from user, and check it                                          
play'' :: String -> String -> IO ()
play'' guesses word = do guess <- getChar
                         checkGuess guess guesses word

-- Prompt for the puzzle word, then start playing, initially with no quessed letters                         
hangman'' :: IO ()
hangman'' = do putStrLn' "Type a word: "
               word <- getLineNoEcho
               putStrLn' "Try to guess letters in the word..."
               putStrLn' "Guess your first letter: "
               play'' [] word                        

-- -------------------------------------------------------------------------------------               
-- Game of "Nim"               

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer _ = 1

validMove :: Int -> Int -> Board -> Bool
validMove row count board = (board !! (row - 1)) >= count

doMove :: Int -> Int -> Board -> Board
doMove row count board = [update r c | (r,c) <- zip [1..] board]
                         where
                             update r c = if row == r then c - count else c

finished :: Board -> Bool
finished = all (== 0)                             

showRow :: Int -> Int -> IO ()
showRow row num = do putStr(show row)
                     putStr " :"
                     putStrLn (concat(replicate num " *"))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do showRow 1 a
                          showRow 2 b                     
                          showRow 3 c                     
                          showRow 4 d                     
                          showRow 5 e                     

newline :: IO ()
newline = putChar '\n'           

getDigit :: String -> IO Int
getDigit prompt = do putStrLn prompt
                     x <- getChar
                     newline
                     if isDigit x then
                         return (digitToInt x)
                     else
                         do putStrLn "Error: not a valid digit"
                            getDigit prompt

nim :: Board -> Int -> IO ()
nim board player = 
    do newline
       putBoard board
       if finished board then
           do newline
              putStr ("Player " ++ (show (nextPlayer player)) ++ " wins!")
       else
           do newline
              putStrLn ("Player " ++ (show (nextPlayer player)))
              row <- getDigit "Enter row number to play: "
              num <- getDigit "Stars to remove: "
              if validMove row num board then
                  nim (doMove row num board) (nextPlayer player)
              else
                  do newline
                     putStrLn "Error: Invalid move"
                     nim board player


-- -------------------------------------------------------------------------------------               
-- "The Game of Life"               
                     
bWidth :: Int
bWidth = 10

bHeight :: Int
bHeight = 10

type Pos = (Int,Int)
type LifeBoard = [Pos]

glider :: LifeBoard
glider = [(3,2),(3,4),(4,3),(4,4),(5,3)]

square :: LifeBoard
square = [(2,2),(3,2),(4,2),
          (2,3),(3,3),(4,3),
          (2,4),(3,4),(4,4)]

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show (2*x) ++ "H")

origin :: IO ()
origin = goto (bWidth+1,bHeight)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

showBoard :: IO ()
showBoard = sequence_ [writeAt (x,y) "." | x <- [1..bWidth], y <- [1..bHeight]]

showCells :: LifeBoard -> IO ()
showCells b = sequence_ [writeAt p "O" | p <- b]

isAlive :: LifeBoard -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: LifeBoard -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` bWidth) + 1,
              ((y-1) `mod` bHeight) + 1)
              
neighbours :: Pos -> [Pos]
neighbours (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1),
                             (x-1,y),            (x+1,y),
                             (x-1,y+1), (x,y+1), (x+1,y+1)]

liveNeighbours :: LifeBoard -> Pos -> Int
--liveNeighbours b p = length [n | n <- neighbours p, isAlive b n]                             
liveNeighbours b = length . filter (isAlive b) . neighbours

survivors :: LifeBoard -> [Pos]
survivors b = [p | p <- b, elem (liveNeighbours b p) [2,3]]

rmdup :: Eq a => [a] -> [a]
rmdup [] = []  
rmdup (x:xs) = x : rmdup (filter (/= x) xs)

births :: LifeBoard -> [Pos] -- Empty cells that are adjacent to 2 live cells
births b = [p | p <- rmdup (concat (map neighbours b)),
                isEmpty b p,
                liveNeighbours b p == 3]

nextgen :: LifeBoard -> LifeBoard
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

life :: LifeBoard -> IO ()
life b = do cls
            showBoard
            showCells b
            origin
            wait 1500000
            life (nextgen b)