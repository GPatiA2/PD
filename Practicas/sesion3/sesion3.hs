-- ej 1

-- a
ej1a n = foldr dup [] [1..n]
   where dup x y = x : (negate x) : y

-- b
ej1b n = [(x, y) | x <- [0..n], y <- [0..n]]


-- ej 2

-- a
sufijos xs = [drop n xs | n <- [0..length xs-1]]

--b
prefijos xs = [take n xs | n <- [1..length xs]]
sublists xs = map sufijos $ prefijos xs

-- c
perms []     = [[]]
perms (x:xs) = concat [intercala x ys | ys <- perms xs]

intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- d

