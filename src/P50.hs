module P50 (
  theAnswer ) where

import Data.Numbers.Primes

thePrimes :: [Integer]
--thePrimes = takeWhile (<4000) primes
thePrimes = take 550 primes

findCombinations :: [Integer] -> [[Integer]]
findCombinations xs = go 1 xs
  where go y ys | y <= length ys = take y ys : go (y + 1) ys
                | otherwise    = []

dropCombinations :: [[Integer]] -> [[Integer]]
dropCombinations = drop 1 . map (drop 1)

filterForPrimes :: [[Integer]] -> [[Integer]]
--filterForPrimes x xs = filter ((`elem` thePrimes x) . sum) xs
filterForPrimes xs = filter (isPrime . sum) xs

findLengths :: [[Integer]] -> [(Int, Integer)]
findLengths = map go
  where go xs = (length xs, sum xs)

calcLength :: [[Integer]] -> [(Int, Integer)]
calcLength xs = let filteredList = filterForPrimes xs
                in findLengths filteredList

calcLength' :: [[Integer]] -> [(Int, Integer)]
calcLength' xs | xs /= [] = calcLength xs ++ calcLength' (dropCombinations xs)
               | otherwise = []

findMil :: Integer -> (Int, Integer) -> Bool
findMil y (_,x) | x <= y = True
                | otherwise = False

theAnswer :: Integer -> (Int, Integer)
theAnswer x = maximum . filter (findMil x) . calcLength' $ findCombinations thePrimes
