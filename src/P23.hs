module P23 (
  p23 ) where

divisors :: Int -> [Int]
divisors x = [a | a <- [1..(x `div` 2)], x `mod` a == 0]

testInt :: Int -> Bool
testInt x = if (sum . divisors) x > x then True else False

abundantNums :: [Int]
abundantNums = filter testInt [12..28123]

testNum :: Int -> Bool
testNum x = if [a+b | a <- abundantNums, b <- abundantNums, a+b == x] /= []
  then False else True

p23 :: Int
p23 = sum . filter testNum $ [1..28123]
