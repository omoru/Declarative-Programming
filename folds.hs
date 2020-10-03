{-
They're sort of like the map function, only they reduce the list to some single value. 

A fold takes a binary function, a starting value (I like to call it the accumulator)
 and a list to fold up. The binary function itself takes two parameters. 
 The binary function is called with the accumulator and the first (or last) element 
 and produces a new accumulator. Then, the binary function is called again with the new 
 accumulator and the now new first (or last) element, and so on.
  Once we've walked over the whole list, only the accumulator remains,
   which is what we've reduced the list to.

-}

                --FOLDL
---------------------------------------------------------

--Empieza con acc = 0 y va sumando empezando por head
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs



{-The lambda function (\acc x -> acc + x) is the same as (+).
We can omit the xs as the parameter because calling foldl (+) 0 will return a function that
takes a list. Generally, if you have a function like foo a = bar b a, you can rewrite it as
 foo = bar b, because of currying.-}
 --version corta
sum2' :: (Num a) => [a] -> a  
sum2' = foldl (+) 0 

--OTRO EJEMPLO
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  



                --FOLDR
----------------------------------------------------------

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  

--FOLDR1 NO HACE FALTA VALOR INICIAL(EMPIEZA EN 1)
---------------------------------------
 
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
      
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
      
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
      
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
      
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
      
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

--Las funciones foldl1 y foldr1 son muy parecidas a foldl y foldr, solo que en lugar que no necesitas indicar un valor de inicio. Asumen que el primer (o el último) 
--elemento de la lista es valor de inicio, luego empiezan a plegar la lista por el elemento siguiente. Por lo tanto la función sumaLista puede ser implementada como: sumaLista = foldl1 (+)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)