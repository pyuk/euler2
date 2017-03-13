module Lib where

import P50 
import P13
import P14
import P15
import P16
import P17
import P19

listAnswers :: Integer -> String
listAnswers x
  | x == 50 = show . snd . theAnswer $ 1000000
  | x == 13 = show p13
  | x == 14 = show $ p14 1000000
  | x == 15 = show $ p15
  | x == 16 = show $ p16
  | x == 17 = show $ p17
  | x == 19 = show $ p19
  | otherwise = "not available"
