module P22 (
  p22 ) where

import Name
import Data.List

assignValue :: [(Char, Int)]
assignValue = zipWith (,) ['A'..'Z'] [1..]

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' a xs = go 0 a xs
  where go n b (y:ys) = if b == y then n else go (n+1) b ys

valueLookUp :: Char -> Int
valueLookUp x = ((map snd assignValue) !! (x `elemIndex'` (map fst assignValue)))

wordValue :: String -> Int
wordValue = sum . map valueLookUp

p22 :: Int
p22 = let sortedNames = sort names
          valuedNames = map wordValue sortedNames
          zippedNames = zip valuedNames [1..]
          individualValues = map (\(a,b) -> a * b) zippedNames
      in sum individualValues
