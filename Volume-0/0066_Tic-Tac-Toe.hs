import Data.Char
import Data.Maybe
import Data.List

data Mark = O | X | S deriving (Eq, Read, Show)
type Board = [[Mark]]

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (_:[]) = True
allEq (x:y:zs)
    | x == y    = allEq (y:zs)
    | otherwise = False

winner :: Board -> Maybe Mark
winner board
    | isJust h  = h
    | isJust v  = v
    | isJust d  = d
    | otherwise = Nothing
    where 
      len = length board - 1
      h = h' board
          where 
            h' [] = Nothing
            h' (x:xs) | allEq x && head x /= S  = Just $ head x
                      | otherwise               = h' xs
      v = v' [0..len]
          where 
            v' [] = Nothing
            v' (n:ns)
                | allEq vl && head vl /= S = Just $ head vl
                | otherwise                = v' ns
                where vl = [board !! m !! n | m <- [0 .. len]]
      d | allEq dlA && head dlA /= S = Just $ head dlA
        | allEq dlB && head dlB /= S = Just $ head dlB
        | otherwise                  = Nothing
        where 
          dlA = [board !! n !! n | n <- [0 .. len]]
          dlB = [board !! n !! n | n <- [len, len-1 .. 0]]

formatting :: String -> [Board]
formatting str = map (spl . map (read . (:[]) . toUpper)) $ lines str
    where spl = unfoldr f
              where f [] = Nothing 
                    f xs = Just $ splitAt 3 xs

result2str :: Maybe Mark -> String
result2str m = case m of Just O  -> "o"
                         Just X  -> "x"
                         Nothing -> "d"

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (result2str . winner) sets

test0066 = putStr . unlines $ map (result2str . winner) sets
    where sets = formatting "ooosxssxs\nxoosxsosx\nooxxxooxo"