import Data.List
import System.IO

data Object = Shore | Sea deriving (Eq, Show)
type Point = (Int, Int)
type IslandMap = [[Object]]
type Island = [Point]

getObj :: Point -> IslandMap -> Object
getObj (x,y) m = m !! y !! x

isInRange :: (Int, Int) -> [[a]] -> Bool
isInRange (x,y) l
    | x < 0 || length (head l) -1 < x = False
    | y < 0 || length l - 1 < y       = False
    | otherwise                       = True

diff :: Eq a => [a] -> [a] -> [a]
diff xs ys = foldl (flip delete) xs ys

shorePts :: IslandMap -> [Point]
shorePts m = [(x,y) | x <- [0..xLen], y <- [0..yLen], 
                getObj (x,y) m == Shore]
    where xLen = length m - 1
          yLen = length (head m) - 1

getIslands :: IslandMap -> [Island]
getIslands m = f shores
    where 
      hLen = length (head m) - 1
      vLen = length m - 1
      shores = shorePts m
      f  [] = []
      f (q:qs)
          | getObj q m == Shore = getIsland q m : f (qs `diff` (getIsland q m))
          | otherwise           = f qs

getIsland :: Point -> IslandMap -> Island
getIsland (h,v) m = f [(h,v)] [(h,v)]
    where 
      f founds [] = founds
      f founds ((x,y):qs)
          | around /= [] = f (founds ++ around) (qs ++ around)
          | otherwise    = f founds qs
          where 
            around = [p | p <- [(x,y-1),(x+1,y),(x,y+1),(x-1,y)],
                      isInRange p m,
                      getObj p m == Shore, not $ p `elem` founds]

nums2map :: [[Int]] -> IslandMap
nums2map nss = row nss
    where row [] = []
          row (y:ys) = column y: row ys
          column [] = []
          column (x:xs)
              | x == 1    = Shore: column xs
              | otherwise = Sea  : column xs

formatting :: String -> [IslandMap]
formatting str = map (nums2map . f) . split $ lines str
    where
      split [] = []
      split ("":ss) = (takeWhile notEmpty ss) : (split $ dropWhile notEmpty ss)
      split ss = (takeWhile notEmpty ss) : (split $ dropWhile notEmpty ss)
      notEmpty x = not $ (==) "" x
      f ss = map (map (read . (:[]))) ss

-- main :: IO ()
-- main = do
--   sets <- fmap formatting getContents
--   putStr . unlines $ map (show . length . getIslands) sets

main' :: FilePath -> IO ()
main' path = do
  withFile path ReadMode $ \handle -> do
      sets <- fmap formatting $ hGetContents handle
      putStr . unlines $ map (show . length . getIslands) sets