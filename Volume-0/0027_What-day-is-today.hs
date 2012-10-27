import Data.Maybe
import Data.List

data Days = Sunday | Monday | Tuesday | Wednesday 
          | Thursday | Friday | Saturday
            deriving (Show, Ord, Eq, Enum)
type Month = Int
type Day = Int

daysList :: [Days]
daysList = [Sunday .. Saturday]

daysOfMonth :: [Int]
daysOfMonth = [ 31, 29, 31, 30, 31, 30
              , 31, 31, 30, 31, 30, 31 ]

dayFromJan1 :: (Month, Day) -> Int
dayFromJan1 (m, d) = d + (sum $ take (m-1) daysOfMonth)

whatDays :: (Month, Day) -> Days
whatDays d = (!!) daysList $ (dayFromJan1 d + 3) `mod` 7

getSets :: String -> [(Month, Day)]
getSets str = map (l2t . map read . words) $ lines str
    where l2t [x,y] = (x,y)

takeBeforeZero :: [(Month, Day)] -> [(Month, Day)]
takeBeforeZero xs = if idx == Nothing then xs 
                    else take (fromJust idx) xs
    where idx = findIndex ((== 0) . fst) xs

main = do
  sets <- fmap getSets getContents
  putStr . unlines . map (show . whatDays) $ takeBeforeZero sets