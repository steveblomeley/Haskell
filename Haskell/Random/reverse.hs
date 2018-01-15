reverseLines input =
  unlines(map reverse (lines input))

main = do
  interact reverseLines