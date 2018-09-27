module Main where

lista = [1..10]

idx :: (Eq a, Num a) => [a] -> a -> a
idx a b | b == 0 = head a
        | otherwise = idx (tail a) (b-1)


main :: IO ()
main = do
  putStrLn "hello world"
