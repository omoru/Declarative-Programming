
-- Devolver true si un elemento esta en una lista y false si no

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = (e == x) || (elem e xs)

-- Crear una funcion nub que elimina todos los duplicados de una lista

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
    | x `elem` xs = nub xs
    | otherwise   = x : nub xs


--Crear una funcion isAsc que devuelva true si es acendiente una lista

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = 
    (x <= y) && isAsc (y:xs)

-- Teniendo una lista de tuplas que son las aristas que unen un grafo dirigido ( por ejemplo la tupla (1,2) 
--quiere decir que hay una arista que va del vertice 1 al 2)
--Dado un nodo inicio y uno final haz una funcion hasPath que nos indicque si existe un camino

hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
 | x == y    = True
 | otherwise =
  let xs' = [ (n,m) | (n,m) <- xs, n /= x ] in 
  or [ hasPath xs' m y | (n,m) <- xs, n == x ]--hacemos un or con todos los elementos de la lista

--v2??

hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath ((n,m):xs) x y = hasPath xs x y || hasPath xs x n && hasPath xs m y

--------------------------------------

--FOLDING EXERCISES

--Crea una funcion rev que haga reverse a una lista

rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

--Crea una funcion prefixes que devuelva todos los prefijos de un alista

prefixes :: [a] -> [[a]]
--prefixes [1,2,3] => [[1],[1,2],[1,2,3]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc) ) []


secsToWeeks secs = let perMinute = 60
                       perHour   = 60 * perMinute
                       perDay    = 24 * perHour
                       perWeek   =  7 * perDay
                   in  secs / perWeek

classify age = case age of 0 -> "newborn"
                           1 -> "infant"
                           2 -> "toddler"
                           _ -> "senior citizen"                 