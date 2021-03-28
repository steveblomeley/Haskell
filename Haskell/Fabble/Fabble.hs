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

-- Do we need alignment? Would imagine it's very rare to have a word that could be
-- entered both Across and Down, starting from the same point . . . but as it could
-- theoretically happen, lets make it explicit always
-- e.g. Move Posn 'A' 12 Alignment Across "BLEEP"
data Alignment = Across | Down deriving Show
data Posn = Posn Char Int deriving Show
type Word = String
data Move = Move Posn Alignment Main.Word deriving Show

