module P13 (
  p13 ) where

import System.IO ( hGetContents, openFile, IOMode ( ReadMode ) )
import System.IO.Unsafe ( unsafePerformIO )

getNums :: IO [String]
getNums = do
  handle <- openFile "src/13.txt" ReadMode
  contents <- hGetContents handle
  --hClose handle
  return $ lines contents

findSum' :: [String] -> Integer
findSum' xs = let nums = sum . map read $ xs
              in connectDigits . take 10 . reverse . sepDigits $ nums
                
findSum :: IO [String] -> IO Integer
findSum xs = do
  nums <- xs
  num <- return $ sum . map read $ nums
  return $ connectDigits . take 10 . reverse . sepDigits $ num

sepDigits :: Integer -> [Integer]
sepDigits 0 = []
sepDigits x = x `mod` 10 : sepDigits (x `div` 10)

connectDigits :: [Integer] -> Integer
connectDigits [] = 0
connectDigits (x:xs) = x * (10 ^ fromIntegral (length xs)) + connectDigits xs

p13 :: Integer
p13 = unsafePerformIO $ getNums >>= return . findSum'
