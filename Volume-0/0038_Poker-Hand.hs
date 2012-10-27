import Data.List

type Cards = (Int, Int, Int, Int, Int)

isSequential :: (Enum a, Eq a) => [a] -> Bool
isSequential [] = True
isSequential (_:[]) = True
isSequential (a:as)
    | succ a == head as = isSequential as
    | otherwise = False

isOnePair :: Cards -> Bool
isOnePair (a,b,c,d,e)
    | length groups == 4 = True
    | otherwise = False
    where groups = group $ sort [a,b,c,d,e]

isTwoPair :: Cards -> Bool
isTwoPair (a,b,c,d,e)
    | length groups == 3 = True
    | otherwise = False
    where groups = group $ sort [a,b,c,d,e]

isThree :: Cards -> Bool
isThree (a,b,c,d,e)
    | (maximum $ map length groups) == 3 = True
    | otherwise = False
    where groups = group $ sort [a,b,c,d,e]

isFour :: Cards -> Bool
isFour (a,b,c,d,e)
    | (maximum $ map length groups) == 4 = True
    | otherwise = False
    where groups = group $ sort [a,b,c,d,e]

isFullHouse :: Cards -> Bool
isFullHouse (a,b,c,d,e)
    | length groups == 2 && (maximum $ map length groups) == 3 = True
    | otherwise = False
    where groups = group $ sort [a,b,c,d,e]

isStraight :: Cards -> Bool
isStraight (a,b,c,d,e)
    | head sorted == 1 && not (sorted !! 1 == 1) && 
      isSequential (tail sorted) = True
    | isSequential sorted = True
    | otherwise = False
    where sorted = sort [a,b,c,d,e]


hands :: Cards -> String
hands c
    | isFour c = "four card"
    | isFullHouse c = "full house"
    | isStraight c = "straight"
    | isThree c = "three card"
    | isTwoPair c = "two pair"
    | isOnePair c = "one pair"
    | otherwise = "null"

getSets :: String -> [Cards]
getSets str = map gs $ lines str
    where
      gs s = (\[a,b,c,d,e] -> (a,b,c,d,e)) $ (\a -> read $ "["++a++"]") s

main = do
  cards <- fmap getSets getContents
  putStr . unlines $ map hands cards
