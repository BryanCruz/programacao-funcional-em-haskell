module Main where

result = res ++ "read*"
  where res = "re" ++ res

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

--11
--data EncodedList = Single Int | Multiple Int Int
--encodeModified ::  [EncodedList]
--encodeModified = [(Single 1)]

--14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

--15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) a = ys ++ (repli xs a)
  where
    ys = repliX a
    repliX 0 = []
    repliX b = x : repliX (b-1) 

--16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryAux xs 1
  where
    dropEveryAux [] _
      | otherwise = []
    dropEveryAux (y:zs) a
      | a == n    = dropEveryAux zs 1
      | otherwise = y : dropEveryAux (zs) (a+1)

--17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n
  | n == 0    = ([], (x:xs))
  | otherwise = (x:ys, zs)
  where
    vs = split xs (n-1)
    ys = fst vs
    zs = snd vs

main :: IO ()
main = do
  putStrLn "hello world"

