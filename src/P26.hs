module P26 (
  p26 ) where

import Unsafe.Coerce

theDec :: Double -> Double
theDec x | x > 0 = 1 / x
         | otherwise = 0

testDec :: Double -> Double
testDec x = 1/x * 100 - (1/x)

p26 :: [Double]
p26 = map theDec [1..10]
