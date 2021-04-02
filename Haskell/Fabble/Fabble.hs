import Data.Char
import System.Random

-- Simple data types - declared to make function type declarations more explicit
boardSize :: Int
boardSize = 15

rackSize :: Int
rackSize = 7

cols :: [Char]
cols = take boardSize ['A'..'Z']

rows :: [Int]
rows = [1..boardSize]

type Tile = Char
type Word = [Tile]
type Bag = [Tile]
type Rack = [Tile]
type Board = [(Position,Tile)]

data Bonus = Word Int | Letter Int

instance Show Bonus where
    show (Word n) 
        | n == 2    = "w"
        | n == 3    = "W"
        | otherwise = "?"
    show (Letter n)
        | n == 2    = "l"
        | n == 3    = "L"
        | otherwise = "?"

l2 :: Bonus
l2 = Letter 2

l3 :: Bonus
l3 = Letter 3

w2 :: Bonus
w2 = Word 2

w3 :: Bonus
w3 = Word 3

bonuses :: [(Position,Bonus)]
bonuses = [(Pos 'A' 1,  w3), (Pos 'H' 1,  w3), (Pos 'O' 1,  w3),
           (Pos 'A' 8,  w3), (Pos 'O' 8,  w3),
           (Pos 'A' 15, w3), (Pos 'H' 15, w3), (Pos 'O' 15, w3),
           (Pos 'B' 2,  w2), (Pos 'N' 2,  w2),
           (Pos 'C' 3,  w2), (Pos 'M' 3,  w2),
           (Pos 'D' 4,  w2), (Pos 'L' 4,  w2),
           (Pos 'E' 5,  w2), (Pos 'K' 5,  w2),
           (Pos 'H' 8,  w2),
           (Pos 'E' 11, w2), (Pos 'K' 11, w2),
           (Pos 'D' 12, w2), (Pos 'L' 12, w2),
           (Pos 'C' 13, w2), (Pos 'M' 13, w2),
           (Pos 'B' 14, w2), (Pos 'N' 14, w2),
           (Pos 'D' 1,  l2), (Pos 'L' 1,  l2),
           (Pos 'G' 3,  l2), (Pos 'I' 3,  l2),
           (Pos 'A' 4,  l2), (Pos 'H' 4,  l2), (Pos 'O' 4,  l2),
           (Pos 'C' 7,  l2), (Pos 'G' 7,  l2), (Pos 'I' 7,  l2), (Pos 'M' 7,  l2),
           (Pos 'D' 8,  l2), (Pos 'L' 8,  l2),
           (Pos 'C' 9,  l2), (Pos 'G' 9,  l2), (Pos 'I' 9,  l2), (Pos 'M' 9,  l2),
           (Pos 'A' 12, l2), (Pos 'H' 12, l2), (Pos 'O' 12, l2),
           (Pos 'G' 13, l2), (Pos 'I' 13, l2),
           (Pos 'D' 15, l2), (Pos 'L' 15, l2),
           (Pos 'F' 2,  l3), (Pos 'J' 2,  l3),
           (Pos 'B' 6,  l3), (Pos 'F' 6,  l3), (Pos 'J' 6,  l3), (Pos 'N' 6,  l3),
           (Pos 'B' 10, l3), (Pos 'F' 10, l3), (Pos 'J' 10, l3), (Pos 'N' 10, l3),
           (Pos 'F' 14, l3), (Pos 'J' 14, l3)]

letterCounts = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]
letterScores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

fullBag :: Bag
fullBag = concat . zipWith replicate letterCounts $ ['A'..'Z']

scores = zip ['A'..'Z'] letterScores

tryFind :: Eq k => k -> [(k,v)] -> Maybe v
tryFind k kvs = if null vs then Nothing else Just (head vs)
                where 
                    vs = [v | (k',v) <- kvs, k' == k]

find :: Eq k => k -> [(k,v)] -> v
find k kvs = head [v | (k',v) <- kvs, k' == k]

-- Data types to describe a move
-- e.g. STDIN> A 12 Across FLIPPER
--    becomes: Move Posn 'A' 12 Alignment Across "FLIPPER"
-- Test in GHCi with: 
--    > Move (Pos a b) c d = read "Move (Pos 'C' 13) Down \"THINGY\"" :: Move
--
-- We will probably need to add "user" to the Move data type

data Alignment = Horizontal | Vertical deriving (Show, Read, Eq)
data Direction = Up | Down | Left | Right deriving Show
data Position = Pos Char Int deriving (Show, Read, Eq)
data Move = Move Position Alignment Main.Word deriving (Show, Read)

-- Randomly pick tiles from bag to rack
randomPick1 :: [a] -> IO a
randomPick1 xs = do
    i <- randomRIO (0, (length xs) - 1)
    return (xs !! i)    

randomPick :: Eq a => [a] -> Int -> IO [a]
randomPick _ 0  = return []
randomPick xs n = do
    x   <- randomPick1 xs
    xs' <- randomPick (xs `without1` x) (n-1)
    return (x : xs')

without1 :: Eq a => [a] -> a -> [a]
without1 [] y = []
without1 (x:xs) y 
   | x == y    = xs
   | otherwise = x : (xs `without1` y)

without :: Eq a => [a] -> [a] -> [a]
without xs []     = xs
without xs (y:ys) = (xs `without1` y) `without` ys 

fillRack :: Rack -> Bag -> IO (Rack,Bag)
fillRack rack bag = do
    ls <- randomPick bag (rackSize - (length rack))
    return (rack ++ ls, bag `without` ls)

testFillRack :: IO ()
testFillRack = do
    print fullBag
    (r,b) <- fillRack [] fullBag
    print r
    print b

-- Basic move validation
onlyAtoZ :: Main.Word -> Bool
onlyAtoZ = foldr (\c b -> b && elem c ['A'..'Z']) True

runsOffBoard :: Move -> Bool
runsOffBoard (Move (Pos col  _) Horizontal word) = lastColOfWord > lastColOnBoard
                                                   where
                                                       lastColOfWord  = ord col + length word - 1
                                                       lastColOnBoard = ord (last cols)
runsOffBoard (Move (Pos _ row) Vertical word) = lastRowOfWord > boardSize
                                                where
                                                    lastRowOfWord = row + length word - 1

checkMove :: Move -> Either String Bool
checkMove (Move (Pos c r) a w) 
    | not (elem r rows)                 = Prelude.Left ("Row should be in the range 1 to " ++ (show boardSize))
    | not (elem c cols)                 = Prelude.Left ("Column should be in the range 'A' to " ++ (show $ last cols))
    | length w < 2                      = Prelude.Left ("Word must be at least 1 letter long")
    | not (onlyAtoZ w)                  = Prelude.Left ("Word should contain only the letters 'A' to 'Z'")
    | runsOffBoard (Move (Pos c r) a w) = Prelude.Left ("That word runs off the edge of the board")
    | otherwise                         = Prelude.Right True

-- Model the board
--
-- How do we add a word to the board?
--
-- First consider how we pass the state of the board between moves
-- - Pass the board itself (i.e. the kv list)
-- - Pass the list of moves so far, and play them against a blank board
-- Let's choose to pass the board initially
--
-- Checks still to be added:
--   Once the word has been added to the board, check that at least one tile
--   has been used from the player's rack, and that the word adjoins letters
--   that are already on the board
--
-- Perpendicular words & dictionary check
--   Search for perpendicular words from each position in the new word.
--   Then check that the played word, plus all perp words are in the dictionary.
--
-- Scoring
--   Then think about scoring
--   Think about bonuses - setup a k,v list of all available bonuses, keyed by 
--   position. Once a bonus is applied, it is removed from the list passed to 
--   to the next turn
--
-- State
--   What state needs to be passed to next turn? 
--   - Board
--   - Bonuses
--   - Racks (pass ALL racks in play - not just the one that has the next turn)
--   - Players' scores
--   - Player whose turn it is next
--
-- Also consider how words played across and words played down could be
-- processed by almost exactly the same code - just need to consider the 
-- board to be rotated by 90 degrees
offBoard :: Position -> Bool
offBoard (Pos col row) = col < head cols || col > last cols || row < head rows || row > last rows 

shift :: Direction -> Int -> Position -> Position
shift Up         n (Pos c r) = Pos c (r - n)
shift Down       n (Pos c r) = Pos c (r + n)
shift Main.Right n (Pos c r) = Pos (chr((ord c) + n)) r
shift Main.Left  n (Pos c r) = Pos (chr((ord c) - n)) r

nextPos :: Alignment -> Position -> Position
nextPos Horizontal = shift Main.Right 1
nextPos Vertical   = shift Down       1

prevPos :: Alignment -> Position -> Position
prevPos Horizontal = shift Main.Left 1
prevPos Vertical   = shift Up        1

addTiles :: Board -> Rack -> Move -> Either String (Board,Rack)
addTiles b r (Move _ _ [])     = Prelude.Right (b,r)
addTiles b r (Move p a (t:ts)) = 
    case tryFind p b of
        Nothing -> if elem t r then 
                       addTiles ((p,t):b) (r `without1` t) remainderOfMove
                   else
                       Prelude.Left "That word needs a tile that isn't on your rack"  
        Just t' -> if t == t' then
                       addTiles b r remainderOfMove
                   else 
                       Prelude.Left "One or more letters in that word do not match tiles already on the board"
    where 
        remainderOfMove = Move (nextPos a p) a ts

isEmpty :: Board -> Position -> Bool
isEmpty b p = tryFind p b == Nothing                     

-- Check a move starts and ends at either the edge of the board, or a blank square
checkWordBoundaries :: Board -> Move -> Bool
checkWordBoundaries b (Move p a w) = (offBoard pBeforeStart || isEmpty b pBeforeStart) &&
                                     (offBoard pAfterEnd    || isEmpty b pAfterEnd)
                                     where
                                         pBeforeStart  = prevPos a p
                                         pAfterEnd     = nextPos a lastPosInWord
                                         lastPosInWord = shift direction (length w) p
                                         direction     = if a == Horizontal then Main.Right else Down

-- Return the adjacent position, in the specified direction                                         
adjacent :: Direction -> Position -> Position
adjacent d = shift d 1

-- Return all letters from a given position that lie in the specified direction
findLetters :: Direction -> Board -> Position -> String
findLetters d b p
    | offBoard p  = []
    | isEmpty b p = []
    | otherwise   = (find p b) : findLetters d b (adjacent d p)       

findLettersUp :: Board -> Position -> String
findLettersUp b p = reverse (findLetters Up b p)
    
findLettersDown :: Board -> Position -> String
findLettersDown = findLetters Down
    
findLettersLeft :: Board -> Position -> String
findLettersLeft b p = reverse (findLetters Main.Left b p)
    
findLettersRight :: Board -> Position -> String
findLettersRight = findLetters Main.Right
    
-- Find a word that crosses perpendicular to an existing word at the specified
-- position
findXWord :: Alignment -> Board -> Position -> String
findXWord Horizontal b p = (findLettersUp b p) ++ tail (findLettersDown b p)
findXWord Vertical   b p = (findLettersLeft b p) ++ tail (findLettersRight b p)

findXWords :: Alignment -> Board -> Position -> [String]
findXWords a b p
    | offBoard p  = []
    | isEmpty b p = []
    | otherwise   = findXWord a b p : findXWords a b (nextPos a p)

-- A turn:
-- TODO: Parse player's move
-- - Prompt for input (Move), parse input and "read" into a Move
-- DONE: Basic validation - checkMove
-- DONE: Add tiles to board - addTiles
-- IN PROGRESS: Find perpendicular words
-- - DONE: findXWords
-- - TODO: filter to remove single letters
-- TODO: Check word connects with at least one existing word on board. Once
--   new tiles added to board . . .
--   - If first move of game, then OK
--   - If # letters used from rack < # letters in word then OK
--   - Otherwise there must be at least one adjoining perpendicular word
-- Check played word and perpendicular words are in dictionary
-- Calculate score

-- Play function
--
-- move <- getMove       -- read the player's move for this turn
-- checkMove move
-- checkWordBoundaries board move
-- addTiles 
-- check that at least one tile was used from player's rack
-- findXWords
-- checkFlow             -- Does the word connect with existing words on board? At this
-- checkDictionary          point we have the info we need to work this out, i.e. is this
-- calculateScore           1st move of game? is word > num tiles used? are there perp words?
-- refillRack
-- play dictionary board bonuses racks scores
--
-- The basic flow we want is:
-- 
-- --> Player A makes move --> Valid move? ---> Yes --> Player B makes move --> Valid move? --> etc ..
--                                         \
--                                          +--> No --> Player A makes move --> Valid move? --> etc ..


-- Print the board, like this:
{-

     A   B   C   D  etc...
   +---+---+---+---+
1  |   | Y |   |   |
   +---+---+---+---+
2  | H | E | L | P |
   +---+---+---+---+
3  |   | L |   |   |
   +---+---+---+---+
4  |   | P |   |   |
   +---+---+---+---+

-}
interleave :: [a] -> [a] -> [a]
interleave [] (y:ys) = [y]
interleave (x:xs) [] = [x]
interleave (x:xs) ys = x : interleave ys xs

printHeader :: IO ()
printHeader = putStrLn ("  "   ++ concat(interleave (repeat "   ") (map (\x -> [x]) cols)))

printDivider :: IO ()
printDivider = putStrLn ("   +" ++ concat(interleave (replicate boardSize "---") (repeat "+")))

squareContent :: Position -> Board -> String
squareContent pos board = case tryFind pos board of
                                  Nothing -> "   "
                                  Just x  -> " " ++ [x] ++ " "                          

rowContents :: Board -> Int -> [String]
rowContents board row = [squareContent (Pos col row) board | col <- cols]

rowNumber :: Int -> String
rowNumber row = n ++ (concat(replicate (3-l) " "))
                where n = show row
                      l = length n 

printRow :: Board -> Int -> IO ()
printRow board row = putStrLn ((rowNumber row) ++ concat(interleave (repeat "|") (rowContents board row)))

printBody :: Board -> Int -> IO ()
printBody board row
    | row > boardSize = do printDivider
    | otherwise       = do printDivider 
                           printRow board row
                           printBody board (row+1)

printBoard :: Board -> IO ()
printBoard b = do
    printHeader
    printBody b 1
