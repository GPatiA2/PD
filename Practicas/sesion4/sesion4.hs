-- Diego Atance Sanz - Ignacio Corrales

-- 1

data Nat = Z | S Nat deriving (Eq, Ord)

(+.) :: Nat -> Nat -> Nat
(+.) Z a = a
(+.) (S a) b = S((+.) a b)

(*.) :: Nat -> Nat -> Nat
(*.) Z a = Z
(*.) (S a) b = (+.) ((*.) a b) b

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

instance Show Nat where
   show Z = "Zero"
   show (S n) = show (1 + (natToInt n))


-- 2

data Complex = C Float Float

instance Num Complex where
   (+) (C a b) (C c d) = (C (a+c) (b+d))
   (-) (C a b) (C c d) = (C (a-c) (b-d))
   (*) (C a b) (C c d) = (C (a*c - b*d) (a*d + b*c))

instance Eq Complex where
   (==) (C a b) (C c d) = ((a == b) && (d == d))
   (/=) (C a b) (C c d) = ((a /= b) || (d /= d))

instance Show Complex where
   show (C a b) = (show a) ++ (show '+') ++(show b) ++ (show 'i')
