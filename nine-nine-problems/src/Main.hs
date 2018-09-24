module Main where

--7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)   = [x] 
flatten (List (x:xs)) = flatten x ++ flatten (List (xs))

--8
compress :: Eq a => [a] -> [a]
compress []    = []
compress [x]   = [x]
compress (x:y:xs) | x == y    = compress (x:xs)
                  | otherwise = x : compress (y:xs)

--9
pack []     = []
pack [x]    = [[x]]
pack (x:xs) = if x == w then [(x : z)] ++ (tail y) else [x] : y
  where
    y = pack xs
    z = head y
    v = tail y
    w = head z

--10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- (pack xs)]

main :: IO ()
main = do
  putStrLn "hello world"

