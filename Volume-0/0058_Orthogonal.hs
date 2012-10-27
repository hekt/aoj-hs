type Point = (Float, Float)
type Line = (Point, Point)
-- data Infinity = Infinity deriving (Eq)

-- slope :: Line -> Either Infinity Float
-- slope ((x1,y1), (x2,y2))
--     | x' == 0 = Left Infinity
--     | otherwise = Right $ y' / x'
--     where x' = x2 - x1
--           y' = y2 - y1

-- isOrthogonal :: (Line, Line) -> Bool
-- isOrthogonal (a, b)
--     | a' == Left Infinity && b' == Right 0 = True
--     | a' == Right 0 && b' == Left Infinity = True
--     | fromRight a' * fromRight b' == (-1)  = True
--     | otherwise                            = False
--       where a' = slope a
--             b' = slope b
--             fromRight = (\(Right n) -> n)

slope :: Line -> Float
slope ((x1,y1), (x2,y2)) = (y2-y1) / (x2-x1)

isOrthogonal :: (Line, Line) -> Bool
isOrthogonal (a, b)
    | a' == inf && b' == 0 = True
    | a' == 0 && b' == inf = True
    | a' * b' == (-1)      = True
    | otherwise            = False
    where a' = slope a
          b' = slope b
          inf = 1/0

yesNo :: Bool -> String
yesNo True = "YES"
yesNo _ = "NO"

formatting :: String -> [(Line, Line)]
formatting str = map (list2line . map read . words) $ lines str
    where
      list2line = (\[x1,y1,x2,y2,x3,y3,x4,y4] -> ( ((x1, y1), (x2, y2))
                                                 , ((x3, y3), (x4, y4)) ) )

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (yesNo . isOrthogonal) sets
