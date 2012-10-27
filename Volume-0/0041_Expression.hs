import Data.List
import Data.Char
import Data.Maybe
import Text.Printf

data Opr = Add | Sub | Mul deriving (Show, Read, Eq)
data ExpKind = ExpA | ExpB deriving (Show, Eq)

opr2func :: Num a => Opr -> (a -> a -> a)
opr2func o
    | o == Add = (+)
    | o == Sub = (-)
    | o == Mul = (*)
opr2str :: Opr -> String
opr2str o
    | o == Add = "+"
    | o == Sub = "-"
    | o == Mul = "*"

evalTo :: Int -> (Int,Int,Int,Int) -> Maybe String
evalTo n (a,b,c,d) 
    | isJust foundA = Just $ formatting ExpA (snd $ fromJust foundA)
    | isJust foundB = Just $ formatting ExpB (snd $ fromJust foundB)
    | otherwise = Nothing
    where
      nss = permutations [a,b,c,d]
      oss = [c:ab | c <- os, ab <- [[a,b] | a <- os, b <- os]]
          where os = [Add, Sub, Mul]
      foundA = find ((==) n . fst) $ calc expA
      foundB = find ((==) n . fst) $ calc expB
      calc e = [(e ns (map opr2func os), (ns,os)) | ns <- nss, os <- oss]
      expA = (\[n1,n2,n3,n4] [o1,o2,o3] -> o3 (o2 (o1 n1 n2) n3) n4)
      expB = (\[n1,n2,n3,n4] [o1,o2,o3] -> o2 (o1 n1 n2) (o3 n3 n4))

formatting :: ExpKind -> ([Int], [Opr]) -> String
formatting ek ((a:b:c:d:_), (o1:o2:o3:_))
    | ek == ExpA = printf "((%d %s %d) %s %d) %s %d" a o1' b o2' c o3' d
    | ek == ExpB = printf "(%d %s %d) %s (%d %s %d)" a o1' b o2' c o3' d
    where (o1', o2', o3') = (opr2str o1, opr2str o2, opr2str o3)

getSets :: String -> [(Int,Int,Int,Int)]
getSets str = gs $ lines str
    where
      gs [] = []
      gs ("0 0 0 0":_) = []
      gs (s:ss) = ((\[a,b,c,d] -> (a,b,c,d)) . map read $ words s): gs ss

main = do
  numsList <- fmap getSets getContents
  let eval ns = evalTo 10 ns
      format n = if isNothing n then "0"
                 else fromJust n
  putStr . unlines $ map (format . eval) numsList