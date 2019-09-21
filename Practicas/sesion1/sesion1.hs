-- ej 1
time :: Integer
time = 10^10

-- 1. a
yearsegs :: Integer
yearsegs = 365 * 24 * 3600
ej1a = (fromIntegral time / fromIntegral yearsegs)

-- 1. b
daysegs :: Integer
daysegs = 24 * 3600
hoursegs :: Integer
hoursegs = 3600
minsegs :: Integer
minsegs = 60

years =  div time yearsegs
days = div (time - (years * yearsegs)) daysegs
hours = div (time - (years * yearsegs) - (days * daysegs)) hoursegs
mins = div (time - (years * yearsegs) - (days * daysegs) - (hours * hoursegs)) minsegs
segs = (time - (years * yearsegs) - (days * daysegs) - (hours * hoursegs) - (mins * minsegs))

ej1b = (years, days, hours, mins, segs)

-- 1. c
ej1c x = let
   years = div x yearsegs
   days = div (x - (years * yearsegs)) daysegs
   hours = div (x - (years * yearsegs) - (days * daysegs)) hoursegs
   mins = div (x - (years * yearsegs) - (days * daysegs) - (hours * hoursegs)) minsegs
   segs = (x - (years * yearsegs) - (days * daysegs) - (hours * hoursegs) - (mins * minsegs))
   in
   (years, days, hours, mins, segs)

-- ej 2
{-
last [1..10^5]                                Evaluable - poco
last [1..10^7]                                Evaluable - poco
last [1..10^20]                               NO evaluable - mucho
head [1..10^20]                               Evaluable - poco
last [10^20..1]                               Exception, empty list (1 no es succ de 10^20)
head (tail [1..10^20])                        Evaluable - poco
length [1..10^20]                             NO evaluable - mucho
last (take (10^7) [1..10^20])                 Evaluable - poco
head (take (10^7) ([1..100] ++ [1..10^20]))   Evaluable - poco
last (take 100 ([1..10^20] ++ [1..100]))      Evaluable - poco
last (drop 100 ([1..10^20] ++ [1..100]))      NO evaluable - mucho
head (drop (10^7) ([1..10^20] ++ [1..100]))   Evaluable poco
[1..10^7]==[1..10^7]                          Evaluable - poco
[1..10^20]==[1..10^20]                        No evaluable - mucho
[1..10^20]==[1..10^20+1]                      No evaluable - mucho
[1..10^20]==[2..10^20]                        NO evaluable - mucho
head (reverse [1..10^7])                      Evaluable - regular
last (reverse [1..10^7])                      Evaluable - regular
reverse [1..10^20] == reverse [1..10^20+1]    Evaluable - poco
-}

-- ej 3
-- Se produce error de tipado dado que / no trabaja con Integer
ej3 xs = let 
   s = sum xs
   l = length xs
   in
   fromIntegral s / fromIntegral l

-- ej 4
--digitos :: Int -> Int
digitos x
  | x < 10 = 1
  | otherwise = 1 + digitos (div x 10)

sumd x
  | x < 10 = x
  | otherwise = (mod x 10) + sumd (div x 10)
-- reduccion :: Int -> Int
reduccion x
  | x < 10 = x
  | otherwise = let y = (sumd x) in if y < 10 then y else reduccion y

-- perm :: Int -> Int
perm x = product [1..x]

-- perm n m :: Int -> Int -> Int
var n m = m^n

-- missing last function

--ej 5
conjBool x False = False
conjBool False x = False
conjBool True x = x
conjBool x True = x
