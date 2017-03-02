module Lib where

import P50 
import P13
import P14

listAnswers :: Integer -> String
listAnswers x | x == 50 = show . snd . theAnswer $ 1000000
              | x == 13 = show p13
              | x == 14 = show $ p14 1000000
              | otherwise = "not available"
