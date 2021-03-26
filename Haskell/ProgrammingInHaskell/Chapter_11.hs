import Data.List

data Player = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

size :: Int
size = 3

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

next :: Player -> Player
next X = O
next O = X
next B = B

-- Not sure why this is needed - if we have "next" ?
turn :: Grid -> Player
turn empty = O
turn g = if os <= xs then O else X
         where 
            os = length (filter (== O) flat)
            xs = length (filter (== X) flat)
            flat = concat g    

-- Get the main diagonal from a grid
-- - First approach gets top LH position, strips first row and column, then recurses
-- - But as we're retrieving [0,0], [1,1] ... [n,n], list comprehension works well
diag' :: Grid -> [Player]
diag' [] = []
diag' (p:ps) = head p : diag' (map tail ps)

diag :: Grid -> [Player]
diag g = [g !! n !!n | n <- [0..size-1]]


wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
           where
               line  = all (== p)
               rows  = g
               cols  = transpose g
               diags = [diag g, diag (map reverse g)] 

won :: Grid -> Bool
won g = wins X g || wins O g
