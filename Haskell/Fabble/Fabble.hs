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
type Word = String
data Move = Move Position Alignment Main.Word deriving (Show, Read)

randomLetters :: [Char] -> Int -> IO [Char]
randomLetters _ 0  = return []
randomLetters cs n = do
    i <- randomRIO (0, (length cs) - 1)
    let x = cs !! i
    xs <- randomLetters (filter (/= x) cs) (n-1)
    return (x : xs)

remove1 :: Eq a => [a] -> a -> [a]
remove1 [] y = []
remove1 (x:xs) y 
   | x == y    = xs
   | otherwise = x : (remove1 xs y)

remove :: Eq a => [a] -> [a] -> [a]
remove xs []     = xs
remove xs (y:ys) = remove (remove1 xs y) ys 

fillRack :: Rack -> Bag -> IO (Rack,Bag)
fillRack rack bag = do
    let lettersNeeded = rackSize - (length rack)
    ls <- randomLetters bag lettersNeeded
    let bag' = remove bag ls
    return (rack ++ ls, bag')

testFillRack :: IO ()
testFillRack = do
    (r,b) <- fillRack [] ['A'..'Z']
    print r
    print b

