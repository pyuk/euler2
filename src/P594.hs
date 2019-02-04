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
searchVertices _ _ _ _ _ = []

matrixElementsM :: [[Int]] -> [Int] -> Int -> Int -> Double
matrixElementsM xs (u:i:j:_) a b = let
  searching x y n = searchVertices xs x y a b !! n
  top' = searching j u 0 - searching i (u-1) 0 +
         searching j u 1 - searching i (u-1) 1
  bottom' = searching j u 0 - searching i (u-1) 0 + j - i
  in combinationFormula (fromIntegral top') (fromIntegral bottom')
matrixElementsM _ _ _ _ = 0

matrixElementsP :: [[Int]] -> [Int] -> Int -> Int -> Double
matrixElementsP xs (v:i:j:_) a b = let
  searching x y n = searchVertices xs x y a b !! n
  top' = searching v j 0 - searching (v-1) i 0 +
         searching v j 1 - searching (v-1) i 0
  bottom' = searching v j 0 - searching (v-1) i 0 + j - i
  in combinationFormula (fromIntegral top') (fromIntegral bottom')
matrixElementsP _ _ _ _ = 0

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
