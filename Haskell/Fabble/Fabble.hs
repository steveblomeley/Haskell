-- Note sure about these . . .
data Bonus = None | Letter Int | Word Int
    deriving (Show)

data Tile = Tile Char Int
    deriving Show    

-- Simple data types - declared to make function type declarations more explicit
type Bag = [Char]
type Rack = [Char]

letterCounts = [9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1]

fullBag :: Bag
fullBag = concat . zipWith replicate letterCounts $ ['A'..'Z']

-- Data types to describe a move
-- e.g. STDIN> A 12 Across FLIPPER
--    becomes: Move Posn 'A' 12 Alignment Across "FLIPPER"
-- Test at GHCi with: 
--    > Move (Pos a b) c d = read "Move (Pos 'C' 13) Down \"THINGY\"" :: Move
--
-- We will probably need to add "user" to the Move data type

data Alignment = Across | Down deriving (Show, Read)
data Position = Pos Char Int deriving (Show, Read)
type Word = String
data Move = Move Position Alignment Main.Word deriving (Show, Read)

