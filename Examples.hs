

-----EJEMPLOS HASKELL----
-- Óscar Morujo Fernaández


{-
                templates
                =========

-DEFINITION

    pattern = result
    ...

-GUARD EXPRESION

    pattern
      |expression = result
      ...
      |otherwise = result

- WHERE CLAUSE: only inside a definition

    result where
    pattern = result
    ...

- LET EXPRESSION: can be used anywhere

    let pattern = result
        ...
    in result

-CASE EXPRESSION : 

    case expression of pattern -> result
                        ...    -> result
                        ...


 -}

iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)
--Prelude> tail [[take 20 (iterate (*x)1)] | x <- [1..20]]

--Devuelve true si el array es creciente
--V1
increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:ys) = x <= y && increasing (y:ys)

--V2
increasing2 :: (Ord a) => [a] -> Bool
increasing2 (x:y:ys) = x <= y && increasing2 (y:ys)
increasing2 _ = True
-------------------------------------------------------

--Recibe una palabra y la devuelve sin vocales
--V1
noVocales :: [Char] -> [Char]
noVocales word = if word == ""
                 then ""
                 else if head word `elem` "aeiouAEIOU"
                      then noVocales (tail word)
                      else (head word) : noVocales (tail word)

--v2
noVocales2 :: [Char] -> [Char]
noVocales2 "" = ""
noVocales2 (x:xs) = if x `elem` "aeiouAEIOU"
                    then noVocales2 xs
                    else x : noVocales2 xs

--v3, con guardas

noVocales3 :: [Char] -> [Char]
noVocales3 "" = ""
noVocales3 (x:xs)
            | x `elem` "aeiouAEIOU" = noVocales3 xs
            | otherwise             = x : noVocales3 xs

-------------------------------------------------------

--Ejemplo de show

watch :: Int -> [Char]
watch n = if n == 7
          then "son las 7!"
          else show n ++ " en punto."

--v2 pattern match

watch2 :: Int -> [Char]
watch2 7 = "son las 7!"
watch2 n = show n ++ " en punto."

--v3 ayuda con variable
watch3 :: Int -> [Char]
watch3 n = show n ++ " en punto y " ++ message n
           where message 7 = "estoy hasta la polla"
                 message _ = "todo ok"

--v4 con ayuda de case
watch4 :: Int -> [Char]
watch4 n = show n ++ " en punto y " ++ case n of 7 -> "estoy hasta la polla"
                                                 _ -> "todo ok"


----------------------------------------------------

gravity :: (Fractional a) => a -> a
gravity r = let g = 6.674e-11
                earthMass = 5.972e24
            in g* earthMass / (r ^ 2)
------------------------------------------------------

--Factorial with Acumulators

fac n = aux n 1
    where aux n acc
           | n <= 1    = acc
           | otherwise = aux (n-1) (n * acc)

---------------------------------------------------

--Funciones con listas intensionales

cuentaCifras lista = [ if x < 10 then "una cifra" else "dos cifras"
                        | x <- lista, odd x]

--Longitud de una lista
longitud lista = sum [ 1 | x <- lista ]

--Cantidad de c's

mostrarC frase = [ letra | letra <- frase, letra == 'c']
sumarC cadena = sum [1 | x <- (mostrarC cadena)]


--------------------------------------------


--DATATYPES

data Calculation = 
    Add Int Int | Sub Int Int | Mul Int
------------------------------------------
data PeaNum = Succ PeaNum | Zero
four :: PeaNum
four = Succ $ Succ $ Succ $ Zero

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum
decr (Succ n) = n


data Tree a = Leaf | Node (Tree a) a (Tree a)
tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)