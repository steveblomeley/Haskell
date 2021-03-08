-- The countdown problem
--

-- First define the four arithmetic operators
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- Functions to determine whether the application of the operator to 2 positive naturals
-- would result in another positive natural, and another to apply the operator
valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x >= y && y /= 1
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Define type for a numeric expression, which can be either an integer value, or the result
-- applying one of the operators to 2 other numeric expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App op left right) = brackets left ++ show op ++ brackets right
                               where
                                   brackets (Val n) = show n
                                   brackets e       = "(" ++ show e ++ ")"
                
-- Functions to return all of the values in an expression, and to evaluate an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op l r) = [apply op x y | x <- eval l, y <- eval r, valid op x y]

-- Return all subsequences of a sequence
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where
                  yss = subs xs

-- Interleave a new member into every possible position of a sequence
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- All permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- All choices of any number of elements from a list, in any order
-- In other words, all permutations of all sub-sequences
allchoices :: [a] -> [[a]]
allchoices = concat . map perms . subs

-- Now can decide if an expression is a solution for a given set of inputs and a target
-- (recall that our evaluate method returns a sequence!)
issolution :: Expr -> [Int] -> Int -> Bool
issolution e is t = elem (values e) (allchoices is) && eval e == [t]

-- Split returns all possible pairs of lists that combine to give the specified list
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []          -- Really?
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Now give the list of all possible operators...
ops :: [Op]
ops = [Add,Sub,Mul,Div]

-- We can generate a list of all expressions that combine 2 expressions using each of the operators
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

----------------------------------------------------------------------------------------------------
-- Performance enhancement
-- A result type that holds an expression and the result of evaluating it
type Result = (Expr, Int)

-- Now for a new version of combine, that combines 2 "results" using each available operator
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- And a results function that gives all possible results whose list of values precisely matches a
-- given list of Ints
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n)]
results ns = [r | (lns,rns) <- split ns, l <- results lns, r <- results rns, r <- combine' l r]

-- And a new solutions function using the above
solutions' :: [Int] -> Int -> [Expr]
solutions' is t = [e | is' <- allchoices is, (e,n) <- results is', n == t]

----------------------------------------------------------------------------------------------------

-- Can now define a function that generates all possible expressions whose values exactly match a specified list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- And can use this to define a function that finds all possible solutions for a countdown problem
-- i.e. expressions that calculate a given target from a combination of the given inputs
solutions :: [Int] -> Int -> [Expr]
solutions is t = [e | is' <- allchoices is, e <- exprs is', eval e == [t]]

main :: IO()   
main = print (solutions' [1,3,7,10,25,50] 765)