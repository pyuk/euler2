module P14 (
  p14 ) where

import Data.Word

collatz :: Word32 -> Word32
collatz x | even x = x `div` 2
          | otherwise = 3 * x + 1

genSeq :: Word32 -> [Word32]
genSeq x | x /= 1 = x : genSeq (collatz x)
         | otherwise = [1]

p14 :: Word32 -> Word32
p14 x = snd . maximum . map (makeTup . genSeq) $ [1..x]
  where makeTup xs = (length xs, head xs)
