module Main where

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = (quickSort smallers) ++ [x] ++ (quickSort greaters)
  where
    smallers = [y | y <- xs, y <  x]
    greaters = [y | y <- xs, y >= x]

primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

primes2 = filterPrime2 [2..10] 
  where filterPrime2 (p:xs) = 
          p : filterPrime2 [x | x <- xs, x `mod` p /= 0]

borzeTest0 = ".-.--"
borzeTest1 = "--."
borzeTest2 = "-..-.--"

borze :: [Char] -> [Char]
borze ('.' : resto)       = ('0' : borze resto)
borze ('-' : '.' : resto) = ('1' : borze resto)
borze ('-' : '-' : resto) = ('2' : borze resto)
borze _ = []

chain ::  Int -> [Int]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd  n = n : chain (n*3 + 1)

numLongChains :: Int -> Int  
numLongChains x = length (filter isLong (map chain [1..1000]))  
    where isLong xs = length xs > x

longestChain :: Int -> (Int, Int)
longestChain n = maxList [chain x | x <- [1..n]]
  where
    maxList []       = (0, 0)
    maxList (xs:xss)
      | (length xs) > (fst maxRec) = (length xs, head xs)
      | otherwise                  = maxRec
      where
        maxRec = maxList xss

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn (borze borzeTest0)
  putStrLn (borze borzeTest1)
  putStrLn (borze borzeTest2)
