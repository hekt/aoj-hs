getNums :: String -> [Int]
getNums [] = []
getNums sss@(s:_)
    | s `elem` ['0'..'9'] = (read $ takeWhile f sss) : 
                            (getNums $ dropWhile f sss)
    | otherwise           = getNums $ dropWhile (not . f) sss
    where f = flip elem ['0'..'9']

secretNumber :: String -> Int
secretNumber str = sum $ getNums str

main = do
  sets <- getContents
  putStrLn . show $ secretNumber sets