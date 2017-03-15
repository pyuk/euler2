module P17 (
  p17 ) where

numWords :: [String]
numWords =["one","two","three","four","five","six","seven","eight","nine",
           "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
           "seventeen","eighteen","nineteen","twenty","thirty","forty","fifty",
           "sixty","seventy","eighty","ninety"]

displayNum :: Int -> String
displayNum x | x < 20 = displayOnes x
             | x >= 20 && x < 100 = displayTens' x
             | x >= 100 && x < 1000 = displayHundreds' x
             | otherwise = "onethousand"

displayOnes :: Int -> String
displayOnes x = numWords !! (x - 1)

displayTens' :: Int -> String
displayTens' x | x `mod` 10 == 0 = numWords !! (19 + (x `div` 10 - 2))
               | otherwise = numWords !! (19 + (x `div` 10 - 2)) ++
                 displayOnes (x `mod` 10)

displayHundreds' :: Int -> String
displayHundreds' x | x >= 100 = displayOnes (x `div` 100) ++ "hundred" ++
                     displayHundreds' (x `mod` 100)
                   | x < 100 && x >= 20 = "and" ++ displayTens' x
                   | x < 20 && x /= 0 = "and" ++ displayOnes x
                   | otherwise = []

p17 :: Int
p17 = length . concat . map displayNum $ [1..1000]
