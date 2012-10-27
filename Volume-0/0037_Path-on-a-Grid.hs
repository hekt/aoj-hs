data Direction = U | D | L | R | N
            deriving (Show, Eq)

type Point = (Int, Int)
type HLines = [[Int]]
type VLines = [[Int]]
type Grid = (HLines, VLines)

-- BEGIN test
-- test0037 :: Bool
-- test0037 = result == "RRRRDDDDLLLUUURRDDLURULLDDDRRRUUUULLLL"
--     where 
--       grid = getSets "1111\n00001\n0110\n01011\n0010\n01111\n0010\n01001\n0111"
--       moves = R: (map fst $ move (R, (2,1)) grid)
--       result = concat . map show $ moves
-- END test

move :: (Direction, Point) -> Grid -> [(Direction, Point)]
move (_, (1,1)) _ = []
move (d, p@(x,y)) g
    | d == R = (moved $ moveR p g): move (moved $ moveR p g) g
    | d == L = (moved $ moveL p g): move (moved $ moveL p g) g
    | d == U = (moved $ moveU p g): move (moved $ moveU p g) g
    | d == D = (moved $ moveD p g): move (moved $ moveD p g) g
    where moved nd
              | nd == R = (R, (x+1,y))
              | nd == L = (L, (x-1,y))
              | nd == U = (U, (x,y-1))
              | nd == D = (D, (x,y+1))

moveR :: Point -> Grid -> Direction
moveR (x, y) (hs, vs)
    | vu == 1 = U
    | h == 1 && vu == 0 = R
    | h == 0 && vd == 1 = D
    | otherwise = L
    where h = hs !! y !! x
          vu = vs !! (y-1) !! x
          vd = vs !! y !! x

moveL :: Point -> Grid -> Direction
moveL (x, y) (hs, vs)
    | vd == 1 = D
    | h == 1 && vd == 0 = L
    | h == 0 && vu == 1 = U
    | otherwise = R
    where h = hs !! y !! (x-1)
          vd = vs !! y !! x
          vu = vs !! (y-1) !! x

moveU :: Point -> Grid -> Direction
moveU (x, y) (hs, vs)
    | hl == 1 = L
    | v == 1 && hl == 0 = U
    | v == 0 && hr == 1 = R
    | otherwise = D
    where hl = hs !! y !! (x-1)
          hr = hs !! y !! x
          v = vs !! (y-1) !! x

moveD :: Point -> Grid -> Direction
moveD (x, y) (hs, vs)
    | hr == 1 = R
    | v == 1 && hr == 0 = D
    | v == 0 && hl == 1 = L
    | otherwise = U
    where hr = hs !! y !! x
          hl = hs !! y !! (x-1)
          v = vs !! y !! x

getSets :: String -> Grid
getSets str = ( [0,0,0,0,0,0]: hLines ls ++ [[0,0,0,0,0,0]]
              , [0,0,0,0,0,0,0]: vLines ls ++ [[0,0,0,0,0,0,0]])
    where
      ls = lines str
      str2ints s = map (read . (:[])) s
      zeroWrap l = [0] ++ l ++ [0]
      hLines [] = []
      hLines (s1:[]) = (zeroWrap $ str2ints s1): []
      hLines (s1:_:ss) = (zeroWrap $ str2ints s1): hLines ss
      vLines [] = []
      vLines (_:[]) = []
      vLines (_:s2:ss) = (zeroWrap $ str2ints s2): vLines ss

main = do
  grid <- fmap getSets getContents
  let result = R: (map fst $ move (R, (2,1)) grid)
  putStrLn . concat $ map show result

