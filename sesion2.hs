--1 FUNCIONES RECURSIVAS
--a) La lista de los cuadrados de los n´umeros naturales entre 0 y n (o sea, [0, 1, 4, 9, . . . , n2]).

cuadrados:: Integral a => a -> [a]
cuadrados n = if n == 0 then [0]
              else cuadrados (n - 1) ++ [n ^ 2]
              

--La lista anterior, pero con cada n´umero emparejado con su cuadrado y en orden inverso ([(n, n2), . . . ,(2, 4),(1, 1),(0, 0)]).

cuadrados':: Integral a => a -> [(a,a)]
cuadrados' n = if n == 0 then [(0,0)]
               else (n,n ^ 2) : cuadrados' (n - 1)
              
-- Suma de i = 1 a i = 100 de  i * abs(sen(i))
--------------------------------------------------------------------------------NO SE PUEDE SUMAR FLOAT + INT, SE HACE CONVERSION ( o pones la i como float com he hehco)
sumatorio :: Float
sumatorio = sumatorioAux n
            where n = 1

sumatorioAux:: Float -> Float
sumatorioAux i
                    | i > 100         =   0
                    | otherwise       =   (i * (abs (sin i))) + (sumatorioAux (i + 1))


-- El n´umero de potencias de 3 que sean menores que n y acaben en 67

potencias3 :: Integer -> Integer
potencias3 n = potencias3Aux n 1
                where potencias3Aux n x
                                       | (3 ^ x) > n  = 0
                                       | 3 ^ x < n && (mod x 100 == 67) = 1 + potencias3Aux n (x + 1)
                                       |otherwise  = potencias3Aux n (x + 1)
        
potencias3' = [3^x | x <-[0..], (3^x < 10^100), (mod (3^x) 100 == 67)]

--La suma de los n´umeros menores que 1000 que sean m´ultiplos de 3 o 5.

suma:: Integer
suma = sumaAux 1
        where sumaAux n
                        | n >= 1000                           = 0
                        | (mod n 3 == 0) || (mod n 5 == 0)    = n + sumaAux (n + 1)
                        |otherwise                            = sumaAux (n + 1)

--2 Programa, utilizando funciones de orden superior predefinidas, las siguientes funciones de orden superior. No olvides declarar sus tipos:
--a) filter2 xs p q = (us, vs) donde us son los elementos de xs que cumplen p y vs los que cumplen q.

filter2:: [a] -> (a -> Bool) -> ( a-> Bool) -> ([a],[a])
filter2 xs p q = (us,vs)
                    where us = filter p xs
                          vs = filter  q xs

--b) filters xs ps = [xs1, . . . , xsn], donde xsi son los elementos de xs que cumplen pi, supuesto que ps es [p1, . . . , pn].

filters:: [a] -> [(a -> Bool)] -> [[a]]
filters xs ps = map (\pi -> filter pi xs) ps

-- c) mapx x [f0,f1,...,fn] = [f0 x,f1 x,...,fn x].

mapx:: a -> [a -> a] -> [a]
mapx x xs = map (\f -> f x) xs -- = mapx x xs = [f x | f<-xs]

-- d) iguales f g n m ⇔ f x = g x, para todo n <= x <= m.

iguales:: (Integral a)=> (a->a) -> (a->a) -> a -> a -> Bool
iguales f g n m = all (\x -> (f x) == (g x)) [n..m]

-- e) cuantos p xs = n´umero de elementos de la lista xs que cumplen la propiedad p.
cuantos :: (a -> Bool) -> [a] -> Int
cuantos p xs = length $ filter p xs

-- f) menorA n m p = menor x con n ≤ x ≤ m que verifica p.
menorA :: Int -> Int -> (Int -> Bool) -> Int
menorA n m p = head $ filter p [n..m]

-- g) mayor n p = mayor x ≤ n que verifica p

mayor :: Int -> (Int -> Bool) -> Int
mayor n p = last $ filter p [0..n]

-- h) ex n m p ⇔ existe x con n ≤ x ≤ m que verifica p.

ex :: Int -> Int -> (Int -> Bool) -> Bool
ex n m p = any p [n..m]


--3) Define mediante foldr o foldl, en lugar de mediante recursion explıcita, las siguientes funciones: last, reverse, all, minimum, map, filter, takeWhile, (++).
--Expresa mediante λ-expresiones el primer argumento de la la funcion fold que utilices.
lastt :: [a] -> a
lastt (x:xs) = foldl (\x y-> y) x (x:xs)

reversee :: [a] -> [a]
reversee (x :xs) = foldl (\x y -> y : x) [] (x:xs)

alll :: (a -> Bool) -> [a] -> Bool
alll p xs = foldl (\x y -> (p y) && x) True xs

-- ///////////////////////////////////////////////////////////////CON FOLD R LOS PARAMETROS ENTRAN AL REVES, LA X ES EL SEGUNDO Y LA Y EL PRIMERO QUE HAY DESPUES
allr :: (a -> Bool) -> [a] -> Bool
allr p xs = foldr (\x y -> (p x) && y) True xs

minimuml :: Ord a => [a] -> a
minimuml (x:xs) = foldl (\x y -> if x < y then x else y) x xs  -- x es el acumulador

minimumr :: Ord a => [a] -> a
minimumr (x:xs) = foldr (\y x-> if x < y then x else y) x xs -- y es el acumulador

mapp :: ( a -> b) -> [a] -> [b]
mapp f = foldr (\x y -> (f x) : y) [] 

mappl  :: ( a -> b) -> [a] -> [b]
mappl f xs = foldl (\x y -> x ++ [(f y)]) [] xs

filterR :: (a -> Bool) -> [a] -> [a]
filterR p xs= foldr (\x y -> if p x then x : y else y ) [] xs --x es xs(cada elemento) , e y es []

filterL :: (a -> Bool) -> [a] -> [a]
filterL p = foldl (\x y -> if p y then x ++ [y] else x ) []

(+++) :: [a]-> [a]-> [a]
(+++) xs ys = foldr (\x y -> x : y) ys xs -- parametro x = xs, paraemtro y = ys

(++++) :: [a]-> [a]-> [a]
(++++) xs ys = foldl (\x y -> x ++ [y]) xs ys

takeWhileL :: (a -> Bool) -> [a] -> [a]
takeWhileL p = foldl (\x y -> if p y then  x ++ [y] else x) []

{-
4. Programa, indicando los tipos, las siguientes variantes de foldl y foldr, que operan
con listas no vac´ıas y no usan valor acumulado inicial:
foldr1 L [x1, . . . , xn] = x1 L x2 L. . .L xn (con L asociando por la derecha)
foldl1 L [x1, . . . , xn] = x1 L x2 L. . .L xn (con L asociando por la izquierda)


-}

foldr1 f = go
  where go [x]            =  x
        go (x:xs)         =  f x (go xs)
        go []             =  errorEmptyList "foldr1"
--example
--foldl1 (\x y -> (x+y)/2) [3,5,10,5] --TOMA EL PRIMER ELEMENTO DE LA LISTA COMO Y, ACTUA DE COMULADOR IMPLICITO.
--Si fuera foldr el acumulador se iniciaria al ultimo elemento
 -- 3+3 / 2 = 3,  5+3 / 2 = 4,  10+4 / 2 = 7,  5+7 / 2 = 6.0





{-
5. Programa al menos tres de los apartados del primer ejercicio utilizando funciones de
orden superior en lugar de recursi´on expl´ıcita.

-}

--a) La lista de los cuadrados de los n´umeros naturales entre 0 y n (o sea, [0, 1, 4, 9, . . . , n2]).

cuadradosOS ::Integral a => a -> [a]
cuadradosOS n = map (^2) [0..n]

              
--La lista anterior, pero con cada n´umero emparejado con su cuadrado y en orden inverso ([(n, n2), . . . ,(2, 4),(1, 1),(0, 0)]).

cuadradosOS':: Integral a => a -> [(a,a)]
cuadradosOS' n = foldl (\x y -> (y,y^2) : x) [] [0..n]
              
-- Suma de i = 1 a i = 100 de  i * abs(sen(i))
sumatorioOS :: Float
sumatorioOS = foldl (\acc i -> acc + i * (abs (sin i))) 0 [1..100]

-- El n´umero de potencias de 3 que sean menores que n y acaben en 67

potencias3OS :: Integer -> Integer
potencias3OS n = foldl (\x y -> if mod y 100 == 67 then x + 1 else x) 0 $ takeWhile (<n) $ map (3^) [0..]

potencias3OS' :: Integer -> [Integer]
potencias3OS' n = foldl (\x y -> if mod y 100 == 67 then y : x else x) [] $ takeWhile (<n) $ map (3^) [0..]



--La suma de los n´umeros menores que 1000 que sean m´ultiplos de 3 o 5.

sumaOS:: Integer
sumaOS = foldr (\x y-> f x y) 0 [1..1000]
            where f x y = if (mod x 3 == 0) ||  (mod x 5 == 0) 
                              then x + y
                              else y



