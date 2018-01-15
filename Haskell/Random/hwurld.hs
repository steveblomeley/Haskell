first = putStrLn "Hellow urld"
second = putStrLn "Time for bed"
third n = putStrLn ("Time for bed, said " ++ (show n))

main = do
    first
    second
    third "Zebedee"
    third 123