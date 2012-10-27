import Data.List
import System.IO

type Point = (Float, Float)
type Vector = (Float, Float)
type Triangle = (Point, Point, Point)

enclosedPins :: [Point] -> [Point]
enclosedPins pts = nub . concat $ map (flip enclosedByTriangle pts)
                   [(a,b,c) | a <- pts, b <- pts, c <- pts,
                    a/=b, a/=c, b/=c, a<b, b<c]

enclosedByTriangle :: Triangle -> [Point] -> [Point]
enclosedByTriangle _ [] = []
enclosedByTriangle tri@(a,b,c) (p:ps)
    | all (> 0) exProds = p : enclosedByTriangle tri ps
    | all (< 0) exProds = p : enclosedByTriangle tri ps
    | otherwise         =     enclosedByTriangle tri ps
    where exProds = map (\(x,y) -> exProd x y) [abp, bcp, cap]
          abp = (pts2vec a b, pts2vec a p)
          bcp = (pts2vec b c, pts2vec b p)
          cap = (pts2vec c a, pts2vec c p)

pts2vec :: Point -> Point -> Vector
pts2vec (ax, ay) (bx, by) = (bx - ax, by - ay)

exProd :: Vector -> Vector -> Float
exProd (ax, ay) (bx, by) = ax * by - bx * ay

formatting :: String -> [[Point]]
formatting str = map f . spl $ lines str
    where
      spl [] = []
      spl ("0":_) = []
      spl sss@(s:ss) = take s' ss : spl (drop s' ss)
          where s' = read s :: Int
      f ss = map (\ s -> read $ "("++s++")") ss

-- main = do
--   sets <- fmap formatting getContents
--   putStr . unlines $ map (show . length . enclosedPins) sets

main' path = do
  withFile path ReadMode $ \handle -> do
      sets <- fmap formatting $ hGetContents handle
      putStr . unlines $ map (show . length . enclosedPins) sets