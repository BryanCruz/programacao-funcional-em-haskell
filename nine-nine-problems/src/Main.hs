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
data EncodedList a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [EncodedList a]
encodeModified []     = []
encodeModified (x:xs) = result : (encodeModified (drop totalEq xs))
  where
    ys      = takeWhile (==x) xs
    totalEq = length ys
    result  = if (totalEq > 0) then (Multiple (totalEq+1) x) else (Single x) 

--12
decodeModified :: [EncodedList a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs)     = x : decodeModified xs
decodeModified ((Multiple a x):xs) = [x | _ <- [1..a]] ++ decodeModified xs


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

--18
slice :: [a] -> Int -> Int -> [a]
slice []     _ _ =     []
slice _      _ 0 =     [] 
slice (x:xs) n m
  | n >  m    = []
  | n <= 1    = if   (m == 1)
                then (x : [])
                else (x : slice xs    1  (m-1))
  | otherwise =  slice xs (n-1) (m-1)
 
--19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n > 0 = rotate ((tail xs) ++ [y]) (n-1)
  | n < 0 = rotate (z : (init xs))    (n+1)
  | otherwise = xs
  where
    y = head xs
    z = last xs

rotate' :: [a] -> Int -> [a]
rotate' xs n
  | n >= 0 = (drop n xs) ++ (take n xs)
  | n <  0 = reverse (rotate' (reverse xs) (-n))

--20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "empty list"
removeAt n (x:xs)
  | n <= 0    = error "invalid index"
  | n == 1    = (x, xs)
  | otherwise = (fst next, x:(snd next))
  where
    next = removeAt (n-1) xs


main :: IO ()
main = do
  putStrLn "hello world"

