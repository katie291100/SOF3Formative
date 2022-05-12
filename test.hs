sqDiff :: [Int] -> [Int]

sqDiff [] = []
sqDiff (x:y:xs)
    | x > y = ((x-y) * (x-y)): sqDiff(y:xs)
    | otherwise = sqDiff(y:xs)
sqDiff [_] = []
