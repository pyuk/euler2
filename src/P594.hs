module P594 (
  p594 ) where

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

vertices :: Int -> Int -> [[(Int,Int)]]
vertices a b = choose (b*b) $ (,) <$> [0..a] <*> [0..a]

arguments :: Int -> [[Int]]
arguments b = (:) <$> [1..b+1] <*> (sequence . replicate 2 $ [1..b])

makeSearchable :: [(Int,Int)] -> [[Int]] -> [[Int]]
makeSearchable xs ys = zipWith (\(a,b) (_:i:j:_) -> [i,j,a,b]) xs ys

combinationFormula :: Double -> Double -> Double
combinationFormula n k
  | n < 0 = 0
  | k < 0 = 0
  | n < k = 0
  | otherwise = fac n `div` (fac k * fac (n-k))
  where fac 1 = 1
        fac n' = n' * fac (n'-1)

searchVertices :: [[Int]] ->  Int -> Int -> Int -> Int -> [Int]
searchVertices [] _ _ _ _= []
searchVertices ((x:x2:xs):xss) i j a b
  | j == 0 = [0,0]
  | i == 0 = [0,a]
  | j == b + 1 = [a,a]
  | i == b + 1 = [a,0]
  | x == i && x2 == j = xs
  | otherwise = searchVertices xss i j a b

matrixElementsM :: [[Int]] -> [Int] -> Int -> Int -> Int
matrixElementsM xs (u:i:j:_) a b = let
  top = searchVertices xs j u a b !! 0 - searchVertices xs i (u-1) a b !! 0 +
        searchVertices xs j u a b !! 1 - searchVertices xs i (u-1) a b !! 1
  bottom = searchVertices xs j u a b !! 0 - searchVertices xs i (u-1) a b !! 0
           + j - i
  in (+) top bottom

matrixElementsP :: [[Int]] -> [Int] -> Int -> Int -> Int
matrixElementsP xs (v:i:j:_) a b = let
  top = searchVertices xs v j a b !! 0 - searchVertices xs (v-1) i a b !! 0 +
        searchVertices xs v j a b !! 1 - searchVertices xs (v-1) i a b !! 1
  bottom = searchVertices xs v j a b !! 0 - searchVertices xs (v-1) i a b !! 0
           + j - i
  in (+) top bottom

matrixBuilderP :: [[Int]] -> [[Int]] -> Int -> Int -> [Int]
matrixBuilderP _ [] _ _ = []
matrixBuilderP v (x:args) a b = matrixElementsP v x a b :
                                matrixBuilderP v args a b 

matrixBuilderM :: [[Int]] -> [[Int]] -> Int -> Int -> [Int]
matrixBuilderM _ [] _ _ = []
matrixBuilderM v (x:args) a b = matrixElementsM v x a b :
                                matrixBuilderM v args a b 

matrixFormater :: Int -> [Int] -> [[[Int]]]
matrixFormater _ [] = []
matrixFormater b xs =
  formatAgain (take (b*b) xs) b : matrixFormater b (drop (b*b) xs)
  where formatAgain [] _ = []
        formatAgain ys b' = take b' ys : formatAgain (drop b' ys) b'
                                  
determinant :: [[Int]] -> Int
determinant [] = 0
determinant [_] = 0
determinant (x:x2:_) = (x !! 0 ) * (x2 !! 1)

runCalc :: Int -> Int -> [[(Int,Int)]] -> [Int]
runCalc _ _ [] = []
runCalc a b (x:xs) = (findProduct . matrixBuilderP arg1 arg2 a $ b) *
                     (findProduct . matrixBuilderM arg1 arg2 a $ b) :
                     runCalc a b xs
  where findProduct = product . map determinant . matrixFormater b
        arg1 = makeSearchable x arg2
        arg2 = arguments b

p594 :: IO ()
p594 = print $ sum . runCalc 3 2 $ (vertices 3 2)
