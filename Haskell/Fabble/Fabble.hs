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
data Alignment = H | V deriving Show
data Posn = Posn Char Int deriving Show
type Word = String
data Move = Move Alignment Posn Main.Word deriving Show

