isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

count :: (a -> Bool) -> [a] -> Int
count f x = length $ filter f x

formatting :: String -> [String]
formatting str = lines str

main = do
  sets <- fmap formatting getContents
  putStrLn . show . count id $ map isPalindrome sets