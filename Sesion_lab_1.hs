
-- 1 a) Cuantos años hay en 10 ^10 segundos

defanios :: Double 
defanios = (10^10 / (3600 * 24 * 365))


--1 b) Cuantos años enteros,dias restantes enteros, horas restantes enteras,min y seg..

calcula :: [Char]
calcula = let anios                 = div (10 ^ 10)  (3600 * 24 * 365)
              segundos_rest_anios   = mod (10 ^ 10) (3600 * 24 * 365)
              dias                  = div segundos_rest_anios (3600 * 24)
              segundos_rest_dias    = mod segundos_rest_anios (3600 * 24)
              horas                 = div segundos_rest_dias 3600
              segundos_rest_horas   = mod segundos_rest_dias 3600
              minutos               = div segundos_rest_horas 60
              segundos              = mod segundos_rest_horas 60
           in "Anios: " ++ show anios ++ ", dias: " ++ show dias ++ " ,horas: " ++ show horas ++ ", minutos: " ++ show minutos
               ++ ", segundos: " ++ show segundos

--1c) Las anteriores con parametro

defanios':: Double -> Double
defanios' n = ( n / (3600 * 24 * 365))




calcula':: Integer -> [Char]
calcula' n = let anios                 = div n  (3600 * 24 * 365)
                 segundos_rest_anios   = mod n (3600 * 24 * 365)
                 dias                  = div segundos_rest_anios (3600 * 24)
                 segundos_rest_dias    = mod segundos_rest_anios (3600 * 24)
                 horas                 = div segundos_rest_dias 3600
                 segundos_rest_horas   = mod segundos_rest_dias 3600
                 minutos               = div segundos_rest_horas 60
                 segundos              = mod segundos_rest_horas 60
             in "Anios: " ++ show anios ++ ", dias: " ++ show dias ++ " ,horas: " ++ show horas ++ ", minutos: " ++ show minutos
                ++ ", segundos: " ++ show segundos


----2
{-
last [1..10^5] -> tiene que construirla para conseguir el ultimo, cuanto mayor sea mas tiempo
last [1..10^7] -> mas aun
last [1..10^20] -> mas todavia
head [1..10^20] -> al ser lazy solo construye y pilla el primero
last [10^20..1] ->  = [] ,No se puede crear listas decrecientes así
head (tail [1..10^20]) -> al ser perezoso solo elimina el primer elemnto y coge head, no tarda casi na
length [1..10^20] -> mucho, tiene que construir la lista como las primeras
last (take (10^7) [1..10^20]) -> al ser perezoso solo construye hasta 10 ^ 70 y coge el ultimo
head (take (10^7) ([1..100] ++ [1..10^20])) ->casi nada ya que al hacer head va a coger el 1
last (take 100 ([1..10^20] ++ [1..100])) -> construye solo 100 elementos y coge el ultimo 
last (drop 100 ([1..10^20] ++ [1..100])) -> al tener que coger el ultimo va a tener que construir toda la lista, pero al hacer drop te ahorras construir esos elementos (tarda mucho)
head (drop (10^7) ([1..10^20] ++ [1..100])) -> tiene que construir hasta 10 ^ 7 y coger la cabeza
[1..10^7]==[1..10^7] -> a mayor numero mayor tiempo, tiene que construir las listas
[1..10^20]==[1..10^20] -> igual
[1..10^20]==[1..10^20+1] -> igual
[1..10^20]==[2..10^20] -> no tarda na, no hace falta construirlas cuando ves que ya son diferentes en la cabeza
head (reverse [1..10^7])  -> tiene que construir la lista para hacer el reverse, mayor tiempo a mayor numero
last (reverse [1..10^7]) -> tarda mucho, al hacer el reverse construye la lista entera
reverse [1..10^20] == reverse [1..10^20+1] -> tarda mucho

-}

--3 FROMITEGRAL
--fromIntegral :: (Integral a, Num b) => a -> b
media :: Integral a => [a] -> Double
media xs = (fromIntegral suma) / (fromIntegral longitud)
           where  suma     = sum xs
                  longitud = length xs 


-- 4.1 digitos x= numero de dıgitos del numero entero x.

digitos :: Integral a => a -> a
digitos n 
        | n < 10      = 1
        | otherwise   = 1 + digitos (div n 10)


-- 4.2 reduccion x= resultado del proceso de sumar los d ́ıgitos del entero x, sumar losd ́ıgitos del resultado obtenido,
-- y ası sucesivamente hasta obtener un n ́umero menorque 10. La reducci ́on de un entero negativo es la de su valor absoluto.

reduccion :: Integral a => a -> a
reduccion x
         | x < 0     = abs x
         | x < 10    = x
         | otherwise = reduccion $ sumaDigitos x

sumaDigitos :: Integral a => a -> a
sumaDigitos x 
            | x < 10 = x
            | otherwise = mod x 10 + sumaDigitos (div x 10)

-- 4.3 perm n= numero de permutaciones de n elementos = factorial

perm:: (Integral a) => a -> a
perm x
    | x < 0    = error "negativo "
    | x == 0   = 0
    | x == 1   = 1
    |otherwise = x * perm (x - 1)

-- 4.4 var n m= numero de variaciones de n elementos tomados de m en m
var:: (Integral a) => a -> a -> a
var m n = perm n `div` perm (n - m)

-- 4.5 comb n m
comb:: (Integral a) => a -> a -> a
comb m n = perm n `div` y
    where y = perm m * perm (n - m)

{- -- 4.6 Definid la conjuncion booleana por ajuste de patrones, pero de cuatro o cinco formas diferentes, cambiando el numero de ecuaciones, o las combinaciones de patrones True,False, x, ...en cada
 ecuacion, o el orden de ecuaciones, etc. y de manera que almenos haya una version estricta en el primer argumento y otra estricta en el segundo,pero no en el primero.
 Para que coexistan todas definiciones en el mismo programa,dadles nombres (o usa operadores) diferentes.
-}


and1 :: Bool -> Bool -> Bool
and1 True y = y
and1 x True = x
and1 False _ = False
and1 _ False = False



-- EJERCICIO  CRIBA DE ERASTÓTENES
elimina:: Int -> [Int] -> [Int]
elimina n xs = [x | x <- xs, x `mod` n /= 0 ]	-- elimina los múltiplos de 2

criba:: [Int] -> [Int]
criba [] = []
criba (n:ns) = n : criba (elimina n ns)

primos:: [Int]		-- Imprime la lista de los números primos
primos = criba [2..] 

esPrimo:: Int -> Bool	-- verifica si n es primo
esPrimo n = head (dropWhile (<n) primos) == n

-- EJERCICIO 11
last_elem:: Eq a => [a] -> a
last_elem (x:xs)
    | xs == []         = x
    | otherwise        = last_elem xs
init_list:: Eq a => [a] -> [a]
init_list (x:xs)
    | xs == []         = []
    | otherwise        = (x:init_list xs)
init_last:: Eq a => [a] -> ([a],a)
init_last xs = (init_list xs, last_elem xs)
concat_list:: [[a]] -> [a]
concat_list = foldr (++) []
take_on_list:: (Eq n, Num n) => n -> [a] -> [a]
take_on_list _ []           = []
take_on_list 0 _            = []
take_on_list n (x:xs)       = x : take_on_list (n-1) xs
drop_on_list:: (Eq n, Num n) => n -> [a] -> [a]
drop_on_list _ []           = []
drop_on_list 0 xs           = xs
drop_on_list n (x:xs)       = drop_on_list (n-1) xs 
split_at_list:: (Eq n, Num n) => n -> [a] -> ([a],[a])
split_at_list n xs = (take_on_list n xs, drop_on_list n xs)
nub_on_list:: (Eq a) => [a] -> [a]
nub_on_list xs              = nub' xs []
    where 
        nub' [] _               = []
        nub' (x:xs) ls 
            | elem x ls         = nub' xs ls
            | otherwise         = x : nub' xs (x:ls)


 