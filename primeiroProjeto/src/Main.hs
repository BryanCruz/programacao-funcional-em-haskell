module Main where

dobra x = 2 * x
quadriplica x = dobra (dobra x)

somatorio n = sum [1..n]
fatorial n = product [1..n]

raiz2Grau :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2Grau a b c = if (delta >= 0) then (x1, x2) else (0, 0)
  where
    x1      = ((-b) + sqDelta ) / (2 * a)
    x2      = ((-b) - sqDelta ) / (2 * a)
    sqDelta = sqrt delta
    delta   = b^2 - 4*a*c

euclidiana :: Floating a => a -> a -> a
euclidiana x y = sqrt diffSq
  where
    diffSq = (x - y)^2

myAbs :: (Ord a, Num a) => a -> a
myAbs n = ( if ( n >= 0 ) then (n) else (-n) )

mySignum :: (Ord a, Num a) => a -> a
mySignum n | n == 0    = 0
           | n > 0     = 1
           | otherwise = -1

mul :: (Eq a, Num a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul x 1 = x
mul 1 y = y
mul x y = x*y

somaMultX :: Num a => a -> (a -> a)
somaMultX x = \y -> x + x * y

main :: IO ()
main = do
  putStrLn "ls -ln"

