data Tree a = Leaf a | Node (Tree a) a (Tree a)

contains :: Eq a => a -> Tree a -> Bool
contains x (Leaf y)     = x == y
contains x (Node l y r) = (x == y) || (contains x l) || (contains x r)

contains' :: Ord a => a -> Tree a -> Bool
contains' x (Leaf y) = x == y
contains' x (Node l y r)
    | x == y    = True
    | x < y     = contains' x l
    | otherwise = contains' x r

multiplyIntLists :: Num i => [i] -> [i] -> [(i,i,i)]
multiplyIntLists lx ly = do
    x <- lx
    y <- ly
    return (x,y,x + y)

    data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)


type Pos = (Int,Int)

data Move = North | East | South | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move East (x,y) = (x+1,y)
move South (x,y) = (x,y-1)
move West (x,y) = (x-1,y)

-- ===================================================================================================
-- 8.6 Tautology Checker
-- Test whether a logical proposition is tautological (ie True for all combinations of input values)

-- Define the "proposition" data type
data Prop = Const Bool 
     | Var Char 
     | Not Prop
     | And Prop Prop
     | Imply Prop Prop
     deriving (Eq, Ord, Show, Read)

-- Define four propositions - we aim to test whether each of these is a tautology
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Create a lookup table type, to allow us to substitute a value (Bool) for a variable name (Char)
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

-- Function to find the value associated with (the first instance in the table of) a specified key
find :: Eq k => k -> Assoc k v -> v       
find k a = head [v | (k',v) <- a, k == k']

-- Define a function to evaluate a proposition given a table of variable substitutions
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find c s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = eval s p1 && eval s p2
eval s (Imply p1 p2) = eval s p1 <= eval s p2

-- Function to find all of the variables used in a proposition
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

-- Create a table of all possible combinations of N boolean values
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True:) bss ++ map (False:) bss
          where bss = bools (n-1)

-- Return unique values from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- Now taking proposition p4 as an example, we have:
-- rmdups (vars p4) = "AB"
-- bools 2          = [[True,True],[True,False],[False,True],[False,False]]
-- We want to convert this into the 4 possible sets of substitutions:
-- [[('A',True),('B',True)], [('A',True),('B',False)], [('A',False),('B',True)], [('A',False),('B',False)]]
-- Then evaluate the proposition p4 for each set of subtitutions, returning True if all 4 evaluate to:t  True, False otherwise

-- First step - zip the list of variables together with each possible combination of boolean variables
makesubs :: [Char] -> [[Bool]] -> [Subst]
makesubs _ [] = []
makesubs cs (bs:bss) = zip cs bs : makesubs cs bss

-- Now we can And the results of evaluating a proposition for each possible set of variable values
taut :: Prop -> Bool
taut p = and [eval s p | s <- ss]
         where
             ss = makesubs vs bs
             bs = bools (length vs)
             vs = rmdups (vars p)

-- Or more concisely . . .
makesubs' :: Prop -> [Subst]
makesubs' p = map (zip vs) (bools (length vs))
              where vs = rmdups (vars p)

taut' :: Prop -> Bool
taut' p = and [eval s p | s <- ss]
          where ss = makesubs' p


-- ===================================================================================================
-- 8.7 Abstract Machine
-- Define an "abstract machine" for expressions, which specifies the process for their evaluation

-- Define  arithmetic expression (addition only supported), and basic function to evaluate

data Expr = Value Int | Add Expr Expr
            deriving (Eq, Ord, Show, Read)

evalue :: Expr -> Int
evalue (Value n) = n
evalue (Add e1 e2) = evalue e1 + evalue e2

-- First step to implement the abstract machine is to define a "control stacks" type, that can be used
-- to define "a list of operations to be performed by the machine after the current evaluation has been
-- completed"

data Op = EVAL Expr | ADD Int
type Cont = [Op]


