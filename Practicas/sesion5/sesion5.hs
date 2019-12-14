--1
adivinar::Int-> IO ()
adivinar n = do 
  putStr "Escribe el numero que crees que es:" 
  x <- do
       line <- getLine
       return (read line::Int)
  if x== -1 then do putStrLn "No has adivinado el numero"
  else if x>n then do 
                   putStrLn "El numero ha adivinar es menor"
                   adivinar n
  else if x<n then do 
                   putStrLn "El numero ha adivinar es mayor"
                   adivinar n 
  else putStrLn "Has adivinado el numero"
  
--2 
countWords = do
  putStr "Escribe la frase de la cual contar palabras: " 
  line <- getLine
  putStrLn $ "La frase tenia: " ++ (show (length $ words line)) ++ " palabras"
  
--3 
--a
--lines divide el texto en lineas
palabras::String -> IO ()
palabras fichero = do
    texto <- readFile fichero
    print . length . words $ texto

--b
palabras' = do
  putStr "Escribe el nombre del fichero a contar palabras: " 
  fichero <- getLine
  texto <- readFile fichero
  putStrLn $ "El fichero '" ++ fichero ++ "' tiene " ++ show (length . words $ texto) ++ " palabras. "
  

--c
{-
acum = 0
numbers = 0
listNumbers = []
promedio = do
  putStr "Escribe el siguiente numero (-1 para acabar):"
  x <- do
       line <- getLine
       return (read line::Int)
  if x == -1 then do 
                  putStrLn "Ejecucion terminada"
  else do 
       x:listNumbers
-}
--d

--e
calculadora = do 
  putStr "Escribe la linea con el operador y los dos operandos: " 
  comando <- getLine
  let {l = read ((words comando) !! 1)::Float;r = read ((words comando) !! 2)::Float} in 
    case (words comando !! 0) of "+" -> print (l + r)
                                 "-" -> print (l - r)
                                 "*" -> print (l * r)
                                 "/" -> print (l / r)
eval o l r = case o of "+" -> l + r
                       "-" -> l - r
                       "*" -> l * r
                       "/" -> l / r
