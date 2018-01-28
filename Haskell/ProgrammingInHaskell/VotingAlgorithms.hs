-- First past the post
-- Each vote is for first choice candidate only
-- List comprehension to count no. of occurences of each item in list, then sort, & pick highest count

import Data.List

numInstances :: Eq a => a -> [a] -> Int
numInstances a = length . filter (== a)

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : filter (/= x) (rmDups xs)

rankResults :: Ord a => [a] -> [(Int, a)]
rankResults xs = sort [(numInstances candidate xs, candidate) | candidate <- rmDups xs]

firstPastPost :: Ord a => [a] -> a
firstPastPost = snd . last . rankResults


-- Alternative vote
-- Vote for as many candidates as you like, in order of preference
-- The count iteratively removes ALL votes for whichever candidate has the least #1 votes...
-- ...until only one candidate is left in #1 positions == the winner

rank :: Ord a => [[a]] -> [a]
rank = map snd . rankResults . map head

rmEmpty :: Eq a => [[a]] -> [[a]]
rmEmpty = filter (/=[])

eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/=x))

alternativeVote :: Ord a => [[a]] -> a
alternativeVote xs = case rank (rmEmpty xs) of
                         [c]  -> c
                         c:cs -> alternativeVote (eliminate c xs)

