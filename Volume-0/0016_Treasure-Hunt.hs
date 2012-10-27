type Point = (Float, Float)
type Distance = Float
type Angle = Float
type Position = (Point, Angle)

move :: Position -> (Distance, Angle) -> Position
move (p@(x,y), a1) (d, a2) = (add' p (distX, distY), a1+a2)
        where distX = sin radA1 * d
              distY = cos radA1 * d
              radA1 = a1 * pi / 180
              add' (x1,y1) (x2,y2) = (x1+x2, y1+y2)

main = do
  input <- fmap lines getContents
  let nums = map (\s -> read ("("++s++")")) input :: [(Distance, Angle)]
      result = foldl move ((0,0),0) nums
  putStr . unlines . map (show . truncate) $ (\((a,b),_) -> [a,b]) result