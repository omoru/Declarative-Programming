-- 1) Definid expresiones Haskell usando funciones de orden superior y/o listas intensionales para representar:

--a) La lista [1, −1, 2, −2, 3, −3, 4, −4, . . .]

numbers = foldl (\x y -> x ++ [y,-y]) [] [1..100]

--b) Una lista infinita [(0, 0),(0, 1),(1, 0),(0, 2),(1, 1),(2, 0),(0, 3),(1, 2), . . .], que sirva
--como enumeraci´on de todas las parejas de n´umeros naturales


---2)
--Programa las siguientes funciones, usando orden superior y listas intensionales.


--a) sufijos xs devuelve las lista de todos los sufijos de xs.
sufijos :: [a]->[[a]]
sufijos = foldr f []
                    where f x [] = [[x]]
                          f x (r:res) = (x:r) : (r:res) 

  
-- b) sublists xs devuelve las lista de todas las sublistas de xs.

-- c) perms xs devuelve la lista de todas las permutaciones de xs.

-- d)sumandos n devuelve la lista de todas las descomposiciones en sumandos positivos de n. Ejemplo: sumandos 3 = [[1,1,1],[1,2],[3]].














































    

