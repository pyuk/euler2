module P16 (
  p16 ) where

powerTwo :: Integer -> Integer
powerTwo x = 2 ^ x

splitNum :: Integer -> [Integer]
splitNum 0 = []
splitNum x = x `mod` 10 : splitNum (x `div` 10)

p16 :: Integer
p16 = sum . splitNum . powerTwo $ 1000
