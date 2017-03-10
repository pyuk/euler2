module P17 (
  p17 ) where

numWords :: [String]
numWords =["one","two","three","four","five","six","seven","eight","nine",
           "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
           "seventeen","eighteen","nineteen"]

displayNum :: Int -> String
displayNum x | x < 20 = displayOnes x
             | x >= 20 && x < 100 = concat $ displayTens x
             | x >= 100 && x < 1000 = concat  $ displayHundreds' x
             | otherwise = "onethousand"

displayOnes :: Int -> String
displayOnes x = numWords !! (x - 1)

displayTens :: Int -> [String]
displayTens 0 = []
displayTens x
  | x < 20 = displayOnes x : []
  | x >= 20 && x < 30 = "twenty" : displayTens (x `mod` 10) 
  | x >= 30 && x < 40 = "thirty" : displayTens (x `mod` 10)
  | x >= 40 && x < 50 = "forty" : displayTens (x `mod` 10)
  | x >= 50 && x < 60 = "fifty" : displayTens (x `mod` 10)
  | x >= 60 && x < 70 = "sixty" : displayTens (x `mod` 10)
  | x >= 70 && x < 80 = "seventy" : displayTens (x `mod` 10)
  | x >= 80 && x < 90 = "eighty" : displayTens (x `mod` 10)
  | x >= 90 && x < 100 = "ninety" : displayTens (x `mod` 10)
  | otherwise = []

displayHundreds' :: Int -> [String]
displayHundreds' x | x >= 100 = displayOnes (x `div` 100) : "hundred" :
                     displayHundreds' (x `mod` 100)
                   | x < 100 && x/= 0 = "and" : displayTens x
                   | otherwise = []

p17 :: Int
p17 = length . concat . map displayNum $ [1..1000]
