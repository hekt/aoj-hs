hell :: Int -> Int -> Int
hell debt 0 = debt
hell debt week = hell ((*1000) . ceiling $ d * 1.05 / 1000) (week - 1)
    where d = fromIntegral debt

main = do
  week <- fmap read getLine
  putStrLn . show $ hell 100000 week