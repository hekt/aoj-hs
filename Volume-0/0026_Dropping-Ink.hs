type Table = [[Int]]
type Point = (Int, Int)

initTable :: Table
initTable = replicate 10 $ replicate 10 0

update :: Table -> Point -> Table
update table (x,y) = row table 0
    where
      row [] _ = []
      row (r:rs) y1
          | y1 == y = column r 0: rs
          | otherwise = r: row rs (y1+1)
      column [] _ = []
      column (c:cs) x1
          | x1 == x = (c+1) : cs
          | otherwise = c: column cs (x1+1)

updateAll :: Table -> [Point] -> Table
updateAll table ps = foldl update table ps

bigDrop :: Point -> [Point]
bigDrop (x,y) = [ (x,y-2)
                , (x-1,y-1), (x,y-1), (x+1,y-1)
                , (x-2,y), (x-1,y), (x,y), (x+1,y), (x+2,y)
                , (x-1,y+1), (x,y+1), (x+1,y+1)
                , (x,y+2) ]

midDrop :: Point -> [Point]
midDrop (x,y) = [ (x-1,y-1), (x,y-1), (x+1,y-1)
                , (x-1,y), (x,y), (x+1,y)
                , (x-1,y+1), (x,y+1), (x+1,y+1) ]

smallDrop :: Point -> [Point]
smallDrop (x,y) = [ (x,y-1)
                  , (x-1,y), (x,y), (x+1,y)
                  , (x,y+1) ]

isInRange :: Point -> Bool
isInRange (x,y)
    | x > 9 || 0 > x = False
    | y > 9 || 0 > y = False
    | otherwise = True

dropArea :: Point -> Int -> [Point]
dropArea p n
    | n == 1 = smallDrop p
    | n == 2 = midDrop p
    | n == 3 = bigDrop p

getSets :: String -> [(Point,Int)]
getSets str = gs $ lines str
    where
      format cs = (\(x,y,n) -> ((x,y),n)) $ read ("("++cs++")")
      gs [] = []
      gs (s:ss) = format s : gs ss

count :: Eq a => a -> [a] -> Int
count x xs = length [x| y <- xs, y == x]
          
main = do
  sets <- fmap getSets getContents
  let drops = map (\(p, n) -> dropArea p n) sets
      result = foldl updateAll initTable drops
  putStrLn $ show (count 0 $ concat result) ++ "\n" 
               ++ show (maximum $ concat result)