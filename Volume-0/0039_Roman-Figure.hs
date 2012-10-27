import Data.Maybe

data RomanFigure = I | V | X | L | C | D | M
                   deriving (Enum, Ord, Eq, Read, Show)
type RomanFigures = [RomanFigure]

roman2arabic :: RomanFigure -> Maybe Int
roman2arabic roman = p roman $ zip [I .. M] [1,5,10,50,100,500,1000]
    where
      p rn [] = Nothing
      p rn (t:ts) 
          | rn == fst t = Just (snd t)
          | otherwise = p rn ts

romans2arabic :: RomanFigures -> Int
romans2arabic romans = r2a 0 romans
    where
      r2a _ [] = 0
      r2a temp (r:[]) = (fromJust $ roman2arabic r) + temp
      r2a temp (r1:r2:rs)
          | r2 > r1 = negate (arabic + temp) + r2a 0 (r2:rs)
          | r1 > r2 = (arabic + temp) + r2a 0 (r2:rs)
          | r1 == r2 = r2a (arabic + temp) (r2:rs)
          where arabic = fromJust $ roman2arabic r1

getSets :: String -> [RomanFigures]
getSets str = map (map (read . (:[]))) $ lines str

main = do
  figsList <- fmap getSets getContents
  putStr . unlines $ map (show .romans2arabic) figsList