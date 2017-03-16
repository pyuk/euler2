module P21 (
  p21 ) where

divisors :: Int -> [Int]
divisors x = [a | a <- [1..(x `div` 2)], x `mod` a == 0] 

tester :: [(Int, Int)] -> [Int]
tester [] = []
tester ((x,y):xs) | x `elem` map snd xs && y /= 1 = x : y : tester xs
                | otherwise = tester xs

makeTups :: Int -> [(Int,Int)]
makeTups 0 = []
makeTups x = (x, sum . divisors $ x) : makeTups (x - 1)

makeTups' :: Int -> [(Int, Int)]
makeTups' x = foldr (\a b -> (a, sum . divisors $ a) : b) [] [1..x]

noRepeats :: [Int] -> [Int]
noRepeats = foldr (\a b -> if a `elem` b then b else a:b) []

testTups :: (Int,Int) -> Bool
testTups (x,y) = if x == (sum . divisors) y && y == (sum . divisors) x && x /= y
                 then True else False

p21 :: Int
p21 = sum . map fst . filter testTups . makeTups' $ 10000
