---------------------------------------1 LISTAS INTENSIONALES
--a) [[1, 2, 3, 4, . . . , 20], [1, 4, 9, 16, . . . , 400], [1, 8, 27, . . . , 8000], ..., [1, 2^10, 3^10, . . . , 20^10]]

f1a::[[Integer]]
f1a = [[x ^ i | x <- [1..20]]| i <- [1..10]]

--b) b) [[1, 1, 1, . . . , 1], [2, 4, 8, 16, . . . , 2^10], [3, 9, 27, . . . , 3^10], . . . , [20, 20^2, 20^3, . . . , 20^10]]

f1b::[[Integer]]
f1b = [[x ^ i | i <- [1..10]]| x <- [1..20]]

---------------------------------------2. Elimina, reemplazandolas por funciones auxiliares no locales, las definiciones locales y la λ-abstraccion de la definicion siguiente:
f x y = map (\u -> (g u,g (u+1))) y
    where z = x * last y
          g u = (x+z)*u

--tenemos que arrastrar x z e y. Se puede hacer mas eficiente ahorrandonos g y combinando los parametros x y z como x+z en uno solo
f2 :: Integral a => a -> [a] -> [(a,a)]
f2 x y = map (faux x (x *last y)) y-- EL TERCER ARGUMENTO ('u' en el enunciado) ES IMPLICITO, ES EL ELEMENTO ACTUAL DE LA LISTA Y QUE ENTRA COMO PARAMETRO A LA FUNCION

faux :: Integral a => a ->a -> a->(a,a)
faux x z u= (g x z u, g x z (u+1) )

g :: Integral a => a -> a -> a ->a
g x z u = (x+z)*u



-----------------------------------------------3. Elimina las listas intensionales de las siguientes definiciones, usando map, filter y concat:

--f n = [x*x | x <- [1..n], mod x 2 == 0]
f3a :: Integer -> [Integer]
f3a n = map (\x -> x*x) $ filter (\x-> x `mod` 2 == 0) [1..n]

--g n m = [x+y | x <- [1..n], y <- [x..m]]
f3b ::  Integer -> Integer -> [Integer]
f3b n m =  concat (map (\x -> map (+ x) [x..m] ) [1..n])


h p n m = [x+y | x <- [1..n], p (n-x), y <- [x..m]]
f3c :: Integral a=>  (a -> Bool) -> a -> a -> [a]
f3c p n m =  concat (map (\x -> map (+ x) [x..m] ) (filter (\a->p (n-a)) [1..n]))

--4 LISTAS INTENSIONALES

--a) La lista con los n´umeros entre 19 y 50 emparejados cada uno con la lista de sus
--divisores (excluido el propio n´umero), es decir, la lista:
--[(19, [1]),(20, [1, 2, 4, 5, 10]),(21, [1, 3, 7]), . . . ,(50, [1, 2, 5, 10, 25])]

f4a:: [(Integer,[Integer])]
f4a = [(n,[ x |x <-[1..n-1], mod n x == 0]) | n<-[19..50]]


-- b) La lista de los n´umeros perfectos menores que 1000. Un n´umero es perfecto si es
--igual a la suma de sus divisores (excluido ´el mismo). Por ejemplo, 6 es perfecto,
--pues 6=1+2+3

f4b :: [Int]
f4b = [ n| n<-[1..1000], sum (divisores n) == n]
        where divisores n = [ x |x <-[1..n-1], mod n x == 0]
f4b' :: [Int]
f4b' = [fst a | a<- [(n,sum [ x |x <-[1..n-1], mod n x == 0])  | n<-[1..1000] ] , snd a == fst a ]


-- c) Generaliza los dos apartados anteriores definiendo funciones, para que no dependan
--de n´umeros naturales concretos sino de los argumentos de la funci´on que definas en
--cada caso


--5. Sea minimoDesde p n una funci´on que devuelve el menor natural mayor o igual que n
--que satisface la propiedad p. Programa esta funci´on usando funciones de OS y/o listas
--intensionales. Util´ızala para encontrar el primer primo a partir de 692.
minimoDesde :: (Integer -> Bool) -> Integer -> Integer 
minimoDesde p n = 1 +  (last $ takeWhile (\x -> not (p x)) [n..])

esPrimo:: Integer -> Bool
esPrimo n =
            let xs = [ x |x <-[1..n-1], mod n x == 0]
            in if length xs > 1 then False else True

--minimoDesde (esPrimo) 692 = 701