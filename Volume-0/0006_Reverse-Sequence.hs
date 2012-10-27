main = do
  l <- getLine
  putStrLn $ reverse' l

reverse' :: [a] -> [a]
reverse' xss = rev xss []
    where rev [] y = y
          rev (x:xs) y = rev xs (x:y)