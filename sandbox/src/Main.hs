module Main where

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

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn (borze borzeTest0)
  putStrLn (borze borzeTest1)
  putStrLn (borze borzeTest2)
