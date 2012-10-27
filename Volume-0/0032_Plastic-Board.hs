import Data.List

type XYD = (Int, Int, Int)
data Parallelogram = Rectangle | Rhombus | Square
                     deriving (Eq, Ord, Show)

isSquare :: XYD -> Bool
isSquare (x,y,d)
    | sqD == d' = True
    | otherwise = False
    where sqD = sqrt $ fromIntegral (x^2 + y^2)
          d' = fromIntegral d

isRhombus :: XYD -> Bool
isRhombus xyd@(x,y,d)
    | x == y && (not $ isSquare xyd) = True
    | otherwise = False

isRectangle :: XYD -> Bool
isRectangle xyd
    | isSquare xyd || isRhombus xyd = False
    | otherwise = True

kind :: XYD -> Parallelogram
kind xyd
    | isSquare xyd = Square
    | isRhombus xyd = Rhombus
    | otherwise = Rectangle

rhombusNum :: [XYD] -> Int
rhombusNum xyds = length $ filter (isRhombus) xyds

rectangleNum :: [XYD] -> Int
rectangleNum xyds = length $ filter (isRectangle) xyds

getSets :: String -> [XYD]
getSets str = map str2tup $ lines str
    where str2tup s = read $ "("++s++")"

main = do
  xyds <- fmap getSets getContents
  mapM_ (putStrLn . show) [rectangleNum xyds, rhombusNum xyds]