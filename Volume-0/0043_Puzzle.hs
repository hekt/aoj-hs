import Data.List
import Data.Maybe

data Kind = Triple | Sequence | Chipped | Double deriving (Eq, Show)
type Piece = Int
type PDbl = (Piece, Piece)
type PTpl = (Piece, Piece, Piece)
type Hand = [Piece]

kindPerms :: [[Kind]]
kindPerms = a ++ b ++ c ++ d ++ e
    where a = nub $ permutations (Double: replicate 4 Triple)
          b = nub $ permutations (Double: replicate 4 Sequence)
          c = nub $ permutations [Double, Triple, Triple, Triple, Sequence]
          d = nub $ permutations [Double, Triple, Triple, Sequence, Sequence]
          e = nub $ permutations [Double, Triple, Sequence, Sequence, Sequence]

isTrueHand :: Hand -> Bool
isTrueHand pieces = isTH kindPerms 
    where 
      isTH [] = False
      isTH (ks:kss)
          | isValidKind pieces ks = True
          | otherwise = isTH kss
      isValidKind [] _ = True
      isValidKind _ [] = False
      isValidKind h (k:ks)
          | isJust result = isValidKind (fromJust result) ks
          | otherwise = False
          where result = dropUsed k h

getDouble :: Hand -> Maybe [Piece]
getDouble pieces = double $ oneAndOthers pieces
    where 
      double [] = Nothing
      double ((p,ps): xs)
          | elem p ps = Just [p, p]
          | otherwise = double xs

getTriple :: Hand -> Maybe [Piece]
getTriple pieces = triple $ oneAndOthers pieces
    where 
      triple [] = Nothing
      triple ((p,ps): xs)
          | (length $ elemIndices p ps) >= 2 = Just [p,p,p]
          | otherwise = triple xs

dropUsed :: Kind -> Hand -> Maybe Hand
dropUsed k pieces
    | isJust used = Just $ deletes (fromJust used) pieces
    | otherwise = Nothing
    where 
      f | k == Triple = getTriple
        | k == Sequence = getSequence
        | k == Double = getDouble
      used = f pieces

getSequence :: Hand -> Maybe [Piece]
getSequence pieces = seq $ twoAndOthers pieces
    where 
      seq [] = Nothing
      seq ( ((a:b:_), ys) : yys )
          | succ a == b && elem (succ b) ys = Just [a, b, succ b]
          | succ a == b && elem (pred a) ys = Just [pred a, a, b]
          | ssa == b && elem (succ a) ys    = Just [a, succ a, b]
          | otherwise                       = seq yys
          where ssa = succ $ succ a

isSequential :: (Enum a, Eq a) => [a] -> Bool
isSequential [] = True
isSequential (_:[]) = True
isSequential (a:as)
    | succ a == head as = isSequential as
    | otherwise = False

oneAndOthers :: Eq a => [a] -> [(a, [a])]
oneAndOthers xxs = oao xxs
    where
      oao [] = []
      oao (x:xs) = (x, delete x xxs): oao xs

twoAndOthers :: (Eq a, Ord a) => [a] -> [([a], [a])]
twoAndOthers xxs = nub . concat . map formatting 
                   $ map (\ (x,xs) -> (x, oneAndOthers xs)) os
    where 
      os = oneAndOthers xxs
      formatting (x, yys) = map (\(y,ys) -> (sort [x,y], sort ys)) yys

deletes :: Eq a => [a] -> [a] -> [a]
deletes [] ys = ys
deletes (x:xs) ys = deletes xs (delete x ys)

isLeftPiece :: Piece -> Hand -> Bool
isLeftPiece x xs
    | idcesLen < 4 = True
    | otherwise = False
    where idcesLen = length $ elemIndices x xs

trueHandPieces :: Hand -> [Piece]
trueHandPieces pieces = map head $ filter isTrueHand handVars
    where nums = [n | n <- [1..9], isLeftPiece n pieces]
          handVars = map (\n -> n:pieces) nums

getSets :: String -> [Hand]
getSets str = map ( map (read . (:[])) ) $ lines str

main = do
  sets <- fmap getSets getContents
  let unwords' ss
          | ss == [] = "0"
          | otherwise = unwords ss
  putStr . unlines $ map (unwords' . (map show) . trueHandPieces) sets

-- test
test0043 = 
    putStr . unlines $ map (unwords' . (map show) . trueHandPieces) sets
    where
      unwords' ss
          | ss == [] = "0"
          | otherwise = unwords ss
      sets = getSets "3649596966777\n6358665788577\n9118992346175\n9643871425498\n7755542764533\n1133557799246"
