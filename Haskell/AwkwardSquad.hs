-- IO Monad - note that we cannot decompose beyond bind (>>=) and return, as 
-- unlike other Monads (e.g. List, Either, Maybe) the definitions of these 
-- functions for IO are hidden from us

-- Echo 2 characters - monadic form, with brackets enclosing lambdas
echo2 :: IO ()
echo2 = getChar >>=   (\c  ->
        putChar c >>= (\() ->
        putChar c >>= (\() ->
        putChar '\n' )))

-- The brackets aren't required though, so:        
echo2' :: IO ()
echo2' = getChar   >>= \c  ->
         putChar c >>= \() ->
         putChar c >>= \() ->
         putChar '\n' 

-- Or could use do notation:
echo2'' :: IO ()
echo2'' = do {
    c <- getChar ;
    putChar c ;
    putChar c ;
    putChar '\n' ;
    return () 
}
        
-- Repeat an IO action of unit N times e.g. repeatN 5 (putStrLn "repeated 5 times")
repeatN :: Int -> IO () -> IO ()
repeatN 0 _ = return ()
repeatN n a = a >> repeatN (n-1) a

-- Perform an IO action for each element in a list
forIO :: [a] -> (a -> IO ()) -> IO ()
forIO [] _ = return ()
forIO (x:xs) fa = fa x >> forIO xs fa

-- Perform a sequence of IO actions of a, returning a list of a. e.g. sequence (replicate 10 getChar)
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (a:as) = do { 
    r <- a; 
    rs <- sequenceIO as;
    return (r:rs) 
}

-- And again without do notation
sequenceIO' :: [IO a] -> IO [a]
sequenceIO' [] = return []
sequenceIO' (fa:fas) = 
    fa >>= \r ->
    sequenceIO fas >>= \rs ->
    return (r:rs)