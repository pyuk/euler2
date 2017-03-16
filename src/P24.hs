module P24 (
  p24 ) where

import Data.List

list = [1..9]
aList = [[a,b,c,d,e,f,g,h,i] | a <- list, b <- list, c <- list, d <- list,
         e <- list, f <- list, g <- list, h <- list, i <- list] 

p24 = sort aList !! 1000000
