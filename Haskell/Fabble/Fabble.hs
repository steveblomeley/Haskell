import Data.Char
import System.Random

-- Note sure about these . . .
data Bonus = None | Letter Int | Word Int
    deriving (Show)

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

letterCounts = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]
letterScores = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

fullBag :: Bag
fullBag = concat . zipWith replicate letterCounts $ ['A'..'Z']

scores = zip ['A'..'Z'] letterScores

find :: Eq k => k -> [(k,v)] -> v
find k kvs = head [v | (k',v) <- kvs, k' == k]

-- Data types to describe a move
-- e.g. STDIN> A 12 Across FLIPPER
--    becomes: Move Posn 'A' 12 Alignment Across "FLIPPER"
-- Test in GHCi with: 
--    > Move (Pos a b) c d = read "Move (Pos 'C' 13) Down \"THINGY\"" :: Move
--
-- We will probably need to add "user" to the Move data type

data Alignment = Across | Down deriving (Show, Read)
data Position = Pos Char Int deriving (Show, Read)
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

offBoard :: Move -> Bool
offBoard (Move (Pos col  _) Across word) = lastColOfWord > lastColOnBoard
                                           where
                                               lastColOfWord  = ord col + length word - 1
                                               lastColOnBoard = ord (last cols)
offBoard (Move (Pos _ row)  Down   word) = lastRowOfWord > boardSize
                                           where
                                               lastRowOfWord = row + length word - 1

checkMove :: Move -> Either String Bool
checkMove (Move (Pos c r) a w) 
    | not (elem r rows)             = Left ("Row should be in the range 1 to " ++ (show boardSize))
    | not (elem c cols)             = Left ("Column should be in the range 'A' to " ++ (show $ last cols))
    | not (onlyAtoZ w)              = Left ("Word should contain only the letters 'A' to 'Z'")
    | offBoard (Move (Pos c r) a w) = Left ("That word runs off the edge of the board")
    | otherwise                     = Right True

-- Model the board
-- Initially as kv list - key is tuple of (col,row), value is Char
-- How do we add a word to the board?

-- First need to ask how we pass the state of the board between moves
-- - Pass the board itself (i.e. the kv list)
-- - Pass the list of moves so far, and play them against a blank board
-- It may be simpler to pass the board

-- So we only have one scenario to consider for adding a word to the board:
-- - Adding a new word
--   To do this, we need the board, plus "move", plus players rack
--   Could return Either updated board & rack, or error message
--   Adding a word could start at the origin position for the word
--   The previous square must be either blank, or off edge of board
--   We need to traverse each letter of the word, checking that:
--   - If the position already has a tile, it's the correct letter
--   - If the position is blank, the missing tile is available in the user's
--     rack - add the tile to the board, regenerate rack minus that tile
--   Once we reach end of word, next square must be either blank, or off
--   the edge of the board
--   Now we can search for perpendicular words - by searching from each
--   position in the new word. Then check that the played word, plus all 
--   perpendicular words are found in the dictionary.
--   ...Then think about scoring
--   ...Think about bonuses (maybe another k,v list, where bonuses are
--      removed after being played)
--   ...And finally the state that needs to be passed to next move, a.k.a.
--      the "game" state - board + bonuses + next player

-- Also consider how words played across and words played down could be
-- processed by almost exactly the same code - just need to consider the 
-- board to be rotated by 90 degrees