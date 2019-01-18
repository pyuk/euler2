module P24 (
  p24 ) where

import Data.List

listToNum :: [Int] -> Int
listToNum [] = 0
listToNum (x:xs) = (x*10^(length xs)) + listToNum xs

perms :: [[Int]]
perms = sort $ permutations [0..9]

p24 :: Int
p24 = listToNum $ sort perms !! 999999
