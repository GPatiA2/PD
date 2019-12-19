data Vertice = A|B|C|D|E|F
data Grafo = G [Vertice] [(Vertice, Vertice)]

-- mat_ady g = matriz adyacencia de g
-- conexo g = indica si g es conexo

g1 = G [B, D, E, C] [(D, E), (E, B), (C, B), (E, C)]
g2 = G [D, F, E] [(D, F), (E, D), (D, E), (F, E)]
g3 = G [A, C, D] [(A, C), (C, D), (A, D)]
g4 = G [A, B, C, D] [(A, B), (B, C), (B, D), (C, D)]
g5 = G [D, F, C, A] [(D, F), (F, C), (F, A), (C, A)]
g6 = G [D, F, C, A] [(D, F), (F, C), (F, D), (C, A)]
g7 = G [A, B, C, D, E, F] [(A, D), (A, E), (B, C), (D, B), (D, E), (E, A), (E, F), (F, C)]

ng3 = G [A, C, D] [(A, C), (C, D), (A, F)]

instance Show Vertice where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"

instance Show Grafo where
    show (G a b) = show ((a,  b))

instance Eq Vertice where
    (==) A A = True
    (==) B B = True
    (==) C C = True
    (==) D D = True
    (==) E E = True
    (==) F F = True
    (==) A _ = False
    (==) B _ = False
    (==) C _ = False
    (==) D _ = False
    (==) E _ = False
    (==) F _ = False

    (/=) A A = False
    (/=) B B = False
    (/=) C C = False
    (/=) D D = False
    (/=) E E = False
    (/=) F F = False
    (/=) A _ = True
    (/=) B _ = True
    (/=) C _ = True
    (/=) D _ = True
    (/=) E _ = True
    (/=) F _ = True

instance Eq Grafo where
    (==) (G xs ys) (G xs' ys') = mismoGrafo (G xs ys) (G xs' ys')
    (/=) (G xs ys) (G xs' ys') = mismoGrafo (G xs ys) (G xs' ys')

-- Extra functions

merge :: ([a], [a]) -> [a]
merge (xs, []) = xs
merge ([], ys) = ys
merge ((x:xs), (y:ys)) = x : y : merge (xs, ys)

equivalent :: Eq a => [a] -> [a] -> Bool
equivalent xs ys = and ([elem y xs | y <- ys])

list_ady :: Vertice -> Grafo -> [Vertice]
list_ady v (G xs ys) = [ b | (a, b) <- ys, v == a]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let small = quicksort [a | a <- xs, a <= x]
        big = quicksort [a | a <- xs, a > x]
    in small ++ [x] ++ big

mismoGrafo :: Grafo -> Grafo -> Bool
mismoGrafo (G xs ys) (G xs' ys') = 
    es_grafo (G xs ys) && 
    es_grafo (G xs' ys') && 
    length xs == length xs' && 
    length ys == length ys' &&
    quicksort [length (list_ady x (G xs ys)) | x <- xs] == quicksort [length (list_ady x (G xs' ys')) | x <- xs']
    

sin_repetidos :: Eq a => [a] -> Bool
sin_repetidos [] = True
sin_repetidos [_] = True
sin_repetidos (x:xs) = if elem x xs then False
                                    else sin_repetidos xs

-- Asked functions

-- es_grafo g = True si la expresion g (que debe ser de tipo Grafo) representa realmente un grafo, es decir el conjunto de vertices de g no es vacıo y no tiene vertices repetidos, y los elementos del conjunto de arcos de g son realmente arcos dentro del conjunto de vertices de g. Vale False en caso contrario.
es_grafo :: Grafo -> Bool
es_grafo (G xs ys) = 
    xs /= [] && 
    sin_repetidos xs &&
    and ([elem y xs | y <- (merge (unzip ys))])

-- mat_ady g = matriz de adyacencia del grafo g.
mat_ady :: Grafo -> [(Vertice, [Vertice])]
mat_ady (G xs ys) = [(x, list_ady x (G xs ys)) | x <- xs]

-- grados pos g = lista de los grados positivos de los vertices del grafo g
grados_pos :: Grafo -> [(Vertice, Int)]
grados_pos (G xs ys) = [ (v, sum [ 1 | (a, b) <- ys, v == a]) | v <- xs]

-- grados neg g = lista de los grados negativos de los vertices del grafo g
grados_neg :: Grafo -> [(Vertice, Int)]
grados_neg (G xs ys) = [(v, sum [ 1 | (a, b) <- ys, v == b]) | v <- xs]

-- camino lng g v n = lista de los caminos en el grafo g con origen en el vertice v y de longitud n
camino_lng :: Grafo -> Vertice -> Int -> [[[Vertice]]]
camino_lng (G xs ys) a l = filter p [caminosAB (G xs ys) a b l | b <- xs, a /= b]
    where p xs = xs /= []

caminosAB :: Grafo -> Vertice -> Vertice -> Int -> [[Vertice]]
caminosAB (G xs ys) u v 0 = if u == v && elem u xs then [[u]]
    else []
caminosAB (G xs ys) u v 1 = if elem (u, v) ys then [[u,v]]
    else []
caminosAB (G xs ys) u v k = filter p [u:vs | n <- [1..k-1], w <- list_ady u (G xs ys), vs <- (caminosAB (G xs ys)) w v n]
    where p xs = (length xs - 1) == k