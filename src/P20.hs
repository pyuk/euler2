module P20 (
  p20 ) where

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

sepDigits :: Integer -> [Integer]
sepDigits 0 = []
sepDigits x = x `mod` 10 : sepDigits (x `div` 10)

p20 :: Integer
p20 = sum . sepDigits . factorial $ 100
