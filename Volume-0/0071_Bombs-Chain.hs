import System.IO
import Text.Printf

type Plane = [[Object]]
data Object = Bomb | Empty deriving (Eq, Show)

bombRange :: (Int, Int) -> Plane -> [(Int, Int)]
bombRange (x,y) plane = [(w,h) | (w,h) <- range,
                         0 <= w, w < width, 0 <= h, h < height]
    where width = length $ head plane
          height = length plane
          range = [ (x,y-1), (x,y-2), (x,y-3),
                    (x+1,y), (x+2,y), (x+3,y),
                    (x,y+1), (x,y+2), (x,y+3),
                    (x-1,y), (x-2,y), (x-3,y) ]

getObj :: (Int, Int) -> Plane -> Object
getObj (x,y) plane = plane !! y !! x

bombed :: (Int, Int) -> Plane -> Plane
bombed (x,y) plane = row 0 plane
    where row _ [] = []
          row n (r:rs) | n == y    = column 0 r : rs
                       | otherwise = r          : row (n+1) rs
          column _ [] = []
          column m (c:cs) | m == x    = Empty : cs
                          | otherwise = c     : column (m+1) cs

bombsChain :: (Int, Int) -> Plane -> Plane
bombsChain (x,y) plane = f ((x,y): bombRange (x,y) plane) plane
    where
      f [] p = p
      f (q:qs) p
          | getObj q p == Bomb = f (qs ++ bombRange q p) (bombed q p)
          | otherwise          = f qs (bombed q p)

formatting :: String -> [((Int, Int), Plane)]
formatting str = f . tail $ lines str
    where
      f [] = []
      f (_:ss) = ((x,y), nums2plane (map str2nums $ take 8 ss)) : 
                 (f $ drop 10 ss)
          where x = (read . head $ drop 8 ss) - 1
                y = (read $ drop 8 ss !! 1) - 1
      str2nums :: String -> [Int]
      str2nums s = map (read . (:[])) s
      nums2plane :: [[Int]] -> Plane
      nums2plane [] = []
      nums2plane (ns:nss) = rep ns : nums2plane nss
          where 
            rep [] = []
            rep (n:ns)
                | n == 1    = Bomb  : rep ns
                | otherwise = Empty : rep ns

unformatting :: [Plane] -> String
unformatting ps = unlines $ map plane2str ps
    where 
      plane2str :: Plane -> String
      plane2str p = unlines $ map rep p
      rep :: [Object] -> String
      rep [] = []
      rep (x:xs)
          | x == Bomb = '1' : rep xs
          | otherwise = '0' : rep xs

main' path = do
  withFile path ReadMode $ \handle -> do
      sets <- fmap formatting $ hGetContents handle
      putStr . unformatting $ map (\(pt, plane) -> bombsChain pt plane) sets