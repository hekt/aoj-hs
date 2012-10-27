import System.IO
import Data.List
import Data.Maybe

type Lot  = Int
type Bar = [BarObj]
data BarObj = Bar | Empty deriving (Eq, Show)

barToWin :: Lot -> Lot -> [Lot] -> [Bar] -> Maybe (Int, Int)
barToWin lot hit lots bars = f empties
    where
      empties = emptyBars bars
      f [] = Nothing
      f (b:bs)
          | isWinningLot lot hit lots (addBar b bars) = Just b
          | otherwise                                 = f bs

isWinningLot :: Lot -> Lot -> [Lot] -> [Bar] -> Bool
isWinningLot lot hit lots bars
    | lot == winningLot hit lots bars = True
    | otherwise                       = False

winningLot :: Lot -> [Lot] -> [Bar] -> Lot
winningLot hit (l:ls) bars
    | goal l bars == hit = l
    | otherwise          = winningLot hit ls bars

goal :: Lot -> [Bar] -> Lot
goal l [] = l
goal l (b:bs)
    | l' < (length b) && b !! l'        == Bar = goal (succ l) bs
    | l' > 0          && b !! (pred l') == Bar = goal (pred l) bs
    | otherwise                                = goal l        bs
    where l' = pred l

addBar :: (Int, Int) -> [Bar] -> [Bar]
addBar (y,x) bars = f (y,x) 0 bars
    where
      f (y,x) y' (b:bs)
          | y' == y   = f' x 0 b : bs
          | otherwise = b        : f (y,x) (y'+1) bs
      f' x x' (a:as)
          | x == x'   = Bar : as
          | otherwise = a   : f' x (x'+1) as

emptyBars :: [Bar] -> [(Int,Int)]
emptyBars bars = concat $ f 0 bars
    where 
      f _ [] = []
      f idx (b:bs)
          | emptyBar b /= [] = zip (repeat idx) (emptyBar b) : f (idx+1) bs
          | otherwise        = f (idx+1) bs

emptyBar :: Bar -> [Int]
emptyBar [] = []
emptyBar (a:[])
    | a == Empty = 0 : []
    | otherwise  =     []
emptyBar bar@(a:b:_)
    | all (==Empty) [a,b] = 0 : f 1 bar
    | otherwise           =     f 1 bar
    where 
      f idx (x:y:[])
          | all (==Empty) [x,y] = idx : []
          | otherwise           =       []
      f idx (x:y:z:ys)
          | all (==Empty) [x,y,z] = idx : f (idx+1) (y:z:ys)
          | otherwise             =       f (idx+1) (y:z:ys)
      f _ _ = []

nums2bars :: [[Int]] -> [Bar]
nums2bars ns = map f ns
    where
      f [] = []
      f (x:xs)
        | x == 1    = Bar   : f xs
        | otherwise = Empty : f xs

str2nums :: String -> [Int]
str2nums s = map (read . (:[])) s

formatting :: String -> (Lot, Lot, [Lot], [Bar])
formatting str = f $ lines str
    where
      f (a:b:c:d:zs) = (b', c', [1..a'], nums2bars (map str2nums $ take d' zs))
          where a' = read a
                b' = read b
                c' = read c
                d' = read d

main' path = do
  withFile path ReadMode $ \handle -> do
      (lot, hit, lots, bars) <- fmap formatting $ hGetContents handle
      let result
              | isWinningLot lot hit lots bars         = "0"
              | isNothing $ barToWin lot hit lots bars = "1"
              | otherwise = (\ (a,b) -> show (a+1) ++ " " ++ show (b+1)) . 
                            fromJust $ barToWin lot hit lots bars
      putStrLn result