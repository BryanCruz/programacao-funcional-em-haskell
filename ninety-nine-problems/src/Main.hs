module Main where

--1
myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

--2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [_] = error "The list has only 1 element"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

--3
elementAt :: (Ord b, Num b) => [a] -> b -> a
elementAt []     _         = error "Empty list"
elementAt  _     n | n < 1 = error "Index smaller than 1"
elementAt xs     1         = head xs
elementAt (x:xs) n         = elementAt xs (n-1)

--4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--5
myReverse :: [a] -> [a]
myReverse []    = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
    isPalindrome (x:xs) | x == last xs = isPalindrome (init xs)
                    | otherwise    = False
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


data EncodedList a = Single a | Multiple Int a deriving (Show)
--11
encodeModified :: Eq a => [a] -> [EncodedList a]
encodeModified [] = []
encodeModified xs = map eval (encode xs)
  where
    eval (a, x) 
      = if   (a == 1) 
        then (Single x) 
        else (Multiple a x)

--12
decodeModified :: [EncodedList a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs)     = x : decodeModified xs
decodeModified ((Multiple a x):xs) = [x | _ <- [1..a]] ++ decodeModified xs

--13
encodeDirect :: Eq a => [a] -> [EncodedList a]
encodeDirect []     = []
encodeDirect (x:xs) = result : (encodeDirect (drop totalEq xs))
  where
    ys      = takeWhile (==x) xs
    totalEq = length ys
    result  = if (totalEq > 0) then (Multiple (totalEq+1) x) else (Single x) 

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

