module Main where

import Lib

main :: IO ()
main = do
  putStr "enter number: "
  num <- getLine
  print $ listAnswers . read $ num
  
