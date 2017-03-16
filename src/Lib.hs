module Lib where

import P50 
import P13
import P14
import P16
import P17
import P19
import P20
import P21
import P22
import P23
import P24

listAnswers :: Integer -> String
listAnswers x
  | x == 50 = show . snd . theAnswer $ 1000000
  | x == 13 = show p13
  | x == 14 = show $ p14 1000000
--  | x == 15 = show $ p15
  | x == 16 = show $ p16
  | x == 17 = show $ p17
  | x == 19 = show $ p19
  | x == 20 = show $ p20
  | x == 21 = show $ p21
  | x == 22 = show $ p22
  | x == 23 = show $ p23
  | x == 24 = show $ p24
  | otherwise = "not available"
