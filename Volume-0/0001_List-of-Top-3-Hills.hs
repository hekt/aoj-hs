import Data.List

top3 :: [Int] -> [Int]
top3 = take 3 . reverse . sort

main = do
  mountains <- getContents
  let numLines = map read $ lines mountains :: [Int]
  putStr . unlines . map show $ top3 numLines