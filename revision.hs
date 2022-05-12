hw :: String
hw = "hello world"

hwp :: IO()
hwp = putStrLn hw

f2 :: Integer -> Integer
f2 n | n == 0 = 2
     | even n = div 2 n
     | otherwise = n+1

fAll :: Integer -> [Integer]
fAll = iterate f2


inRangeF :: Integer -> [Integer]
inRangeF = takeWhile (not . flip elem [0 .. 2]) . fAll

findStable :: Integer -> Integer
findStable = go [] . fAll
  where
      go seen (x:xs) | x `elem` seen = x
                     | otherwise = go (x: seen) xs
