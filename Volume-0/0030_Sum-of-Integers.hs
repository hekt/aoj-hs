import Control.Applicative
import Data.List

numLists :: Int -> [[Int]]
numLists n = reverse $ nl (n-1)
    where
      nl 0 = [0..9]: []
      nl a = [a..9]: nl (a-1)

combs :: Ord a => [[a]] -> [[a]]
combs [] = [[]]
combs (x:xs) = nub . map sort $ (:) <$> x <*> combs xs

isSumOf :: Int -> [Int] -> Bool
isSumOf n xs = if sum xs == n then True
               else False

hasOverlapped :: Eq a => [a] -> Bool
hasOverlapped [] = False
hasOverlapped (x:xs)
    | x `elem` xs = True
    | otherwise = hasOverlapped xs

getSets :: String -> [(Int, Int)]
getSets str = gs $ lines str
    where 
      gs [] = []
      gs ("0 0":_) = []
      gs (x:xs) = ((\[a,b] -> (read a, read b)) $ words x) : gs xs

sumOf :: Int -> Int -> [[Int]]
sumOf n s = filters . combs $ numLists n
    where filters = filter (isSumOf s) . filter (not . hasOverlapped)

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (show . length . (\(a,b) -> sumOf a b)) sets
