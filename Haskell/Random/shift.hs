shift n
  | n >= 'a' && n <= 'z' = toEnum(fromEnum n - 32)
  | n >= 'A' && n <= 'Z' = toEnum(fromEnum n + 32)
  | otherwise = n

main = do
  interact (map shift)
