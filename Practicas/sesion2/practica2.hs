-- ej 1

-- a
cuadrados n
   | n < 0 = []
   | otherwise = rcuadrados n []

rcuadrados n xs
   | n == 0 = 0 : xs
   | otherwise = rcuadrados (n-1) ((n*n) : xs)

-- b
emcuadrados n
   | n == 0 = [(0,0)]
   | otherwise = (n, n*n) : emcuadrados (n-1)

-- c
sumsen = rsumsen 1

rsumsen i
   | i == 100 = i * abs (sin 100)
   | otherwise = i * abs (sin i) + rsumsen (i+1)

-- d
pot3 n = length (rpot3 n 1 [])

rpot3 n i xs
   | x > n = xs
   | x `mod` 100 == 67 = rpot3 n (i+1) (x:xs)
   | otherwise = rpot3 n (i+1) xs
   where x = 3^i

-- e
summen1000 = rsummen1000 1 0

rsummen1000 i s
   | i == 1000 = s
   | (mod i 3 == 0) = rsummen1000 (i+1) (s+i)
   | (mod i 5 == 0) = rsummen1000 (i+1) (s+i)
   | otherwise = rsummen1000 (i+1) s

-- ej 2

-- a
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q = (us, vs)
   where us = filter p xs
         vs = filter q xs

-- b
filters:: [a] -> [(a->Bool)] -> [[a]]
filters xs ys = map (\f -> filter f xs) ys


-- c
mapx :: t -> [t -> b] -> [b]
mapx x fs = map (\f -> f x) fs

-- d
iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m = y
   where xs = map f [n..m]
         ys = map g [n..m]
         y = all (==True) (zipWith (==) xs ys)

-- e
cuantos :: (a -> Bool) -> [a] -> Int
cuantos p xs = y
   where y = length (filter p xs)

-- f
menorA :: Enum a => a -> a -> (a -> Bool) -> a
menorA n m p = y
   where y = head (filter p [n..m])

-- g
mayor :: (Num a, Enum a) => a -> (a -> Bool) -> a
mayor n p = y
   where y = head (filter p [n, n-1..])

-- h
ex :: Enum a => a -> a -> (a -> Bool) -> Bool
ex n m p = y
   where y = length (filter p [n..m]) > 0

-- ej 3

-- a last 
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- b reverse 
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- c (all)
all':: (a->Bool) -> [a] -> Bool
all' f xs = foldr (\x acc -> f x && acc) True xs


-- d
minimum' :: (Ord a) => [a] -> a
minimum' = foldr1 (\x acc -> if x < acc then x else acc)

-- e
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- f
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- g (takeWhile)
takeWhile' :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhile' f xs = foldr (\x acc -> if f x then x:acc else []) [] xs

-- h (++)
concat':: [a]->[a]->[a]
concat' xs ys = foldr (\x acc -> x:acc) ys xs

-- ej 4
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f (x:xs) = foldr f x xs 

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl f x xs 

-- ej 5

-- a
cuadrados' :: (Num b, Enum b) => b -> [b]
cuadrados' n = map (^2) [0..n]

-- b
emcuadrados' :: (Num b, Enum b) => b -> [(b, b)]
emcuadrados' n = reverse (zip [0..n] (map (^2) [0..n]))

-- d
--pot3' n = filter (<n) ( filter (\x -> f x) $ (iterate (3^) [1..]) ) 
--	where f x = (x `mod` 100) == 67
