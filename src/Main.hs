module Main where

import Lib

main :: IO ()
main = do
  num <- getLine
  print $ listAnswers . read $ num
  
