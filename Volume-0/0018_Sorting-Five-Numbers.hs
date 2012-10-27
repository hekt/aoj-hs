rsort :: Ord a => [a] -> [a]
rsort [] = []
rsort (x:xs) = rsort l ++ [x] ++ rsort s
    where s = [a | a <- xs, a <= x]
          l = [b | b <- xs, b > x]

main = do
  xs <- fmap words getLine
  putStrLn . unwords $ rsort xs