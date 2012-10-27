import Data.Maybe

upperChars :: [Char]
upperChars = ['A'..'Z']

lowerChars :: [Char]
lowerChars = ['a'..'z']

getIndex :: Eq a => a -> [a] -> Maybe Int
getIndex x ys = idx x ys 0
    where
      idx _ [] _ = Nothing
      idx a (b:bs) n
          | a == b = Just n
          | otherwise = idx a (bs) (n+1)

low2up :: Char -> Char
low2up c
    | isJust i = upperChars !! fromJust i
    | otherwise = c
    where i = getIndex c lowerChars

main = do
  s <- getLine
  putStrLn $ map low2up s