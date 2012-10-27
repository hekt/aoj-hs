import Data.List
import Data.Maybe

type Figure = [(Int, Int)]
type Surface = [[Int]]

getFigure :: Surface -> Maybe Figure
getFigure s = if fig == [] then Nothing else Just fig
    where
      fig = concat $ yca 0 s
      yca yn [] = []
      yca yn (x:xs) = xca x: yca (yn+1) xs
          where xca ns = zip (elemIndices 1 ns) (repeat yn)

isFigureA :: Figure -> Bool
isFigureA ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x+1, y)   == (x1, y1) && 
      (x, y+1)   == (x2, y2) && 
      (x+1, y+1) == (x3, y3) = True
    | otherwise = False
isFigureA _ = False

isFigureB :: Figure -> Bool
isFigureB ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x, y+1) == (x1, y1) &&
      (x, y+2) == (x2, y2) &&
      (x, y+3) == (x3, y3) = True
    | otherwise = False
isFigureB _ = False

isFigureC :: Figure -> Bool
isFigureC ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x+1, y) == (x1, y1) &&
      (x+2, y) == (x2, y2) &&
      (x+3, y) == (x3, y3) = True
    | otherwise = False
isFigureC _ = False

isFigureD :: Figure -> Bool
isFigureD ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x-1, y+1) == (x1, y1) &&
      (x, y+1)   == (x2, y2) &&
      (x-1, y+2) == (x3, y3) = True
    | otherwise = False
isFigureD _ = False

isFigureE :: Figure -> Bool
isFigureE ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x+1, y)   == (x1, y1) &&
      (x+1, y+1) == (x2, y2) &&
      (x+2, y+1) == (x3, y3) = True
    | otherwise = False
isFigureE _ = False

isFigureF :: Figure -> Bool
isFigureF ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x, y+1)   == (x1, y1) &&
      (x+1, y+1) == (x2, y2) &&
      (x+1, y+2) == (x3, y3) = True
    | otherwise = False
isFigureF _ = False

isFigureG :: Figure -> Bool
isFigureG ((x,y):(x1,y1):(x2,y2):(x3,y3):[])
    | (x+1, y)   == (x1, y1) &&
      (x-1, y+1) == (x2, y2) &&
      (x, y+1)   == (x3, y3) = True
    | otherwise = False
isFigureG _ = False

getKindOfFigure :: Figure -> Maybe String
getKindOfFigure fig
    | isFigureA fig = Just "A"
    | isFigureB fig = Just "B"
    | isFigureC fig = Just "C"
    | isFigureD fig = Just "D"
    | isFigureE fig = Just "E"
    | isFigureF fig = Just "F"
    | isFigureG fig = Just "G"
    | otherwise = Nothing

getSets :: String -> [Surface]
getSets str = gs $ lines str
    where
      gs [] = []
      gs s = (strs2srf $ take 8 s): (gs $ drop 9 s)
      strs2srf = map (map (read . (:[])))

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ 
       map (fromJust . getKindOfFigure . fromJust . getFigure) sets