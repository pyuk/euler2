module P15 where

--import Data.Word
import Data.List ( intersperse, sort )

takeSomething :: Int -> Int -> [Int]
takeSomething x y = take x (repeat y)

addZeroOrOne :: Bool -> Int
addZeroOrOne x | x = 0
               | otherwise = 1

condense :: [[a]] -> [a]
condense [] = []
condense (xs:xss) = xs ++ condense xss

mappingSomething :: Int -> [[Int]]
mappingSomething 2 = concat $ map (\a -> map (:a:[]) (take 2 . intersperse 1 . repeat $ 0)) (take 2 $ intersperse 1 $ repeat 0)
mappingSomething x = concat $ map (\a -> map (:a) (take x . intersperse 1 . repeat $ 0)) $ mappingSomething (x - 1)
