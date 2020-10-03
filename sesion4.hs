{-
 1 .Definir un tipo para representar numeros complejos y declararlo como instancia de las clases Eq,Num,  usando deriving cuando  sea  conveniente.  Declararlo  tambi ́en  como  instancia 
  de Show de modo que, por ejemplo,show del t́ermino Haskell que represente al complejo 2−3i sea el string "2-3i".(Para  declarar  un  tipotcomo  instancia  de  Show  basta  con  definir
   la  funcion show::t->String, no hace falta definir las otras funciones de la clase Show)
-}


type P_real = Float
type P_imaginaria = Float
data Complejo = Complejo P_real P_imaginaria deriving Eq
instance Show Complejo where
    show (Complejo a b) =  show a ++ (if b > 0 then " + " else " ") ++ show b ++ " i"

--NUM NO SE PUEDE DERVIAR : deriving is only available as defined by the compiler for specific Prelude typeclasses: Read, Show, Eq, Ord, Enum, and Bounded.
instance Num Complejo where
    (+) (Complejo x y) (Complejo x' y') = Complejo (x+x') (y+y')
    (-) (Complejo x y) (Complejo x' y') = Complejo (x-x') (y-y')
    {- WARNING DE QUE FALTAN POR IMPLEMENTAR
            No explicit implementation for
                `*', `abs', `signum', and `fromInteger'
            * In the instance declaration for `Num Complejo'
        |
        14 | instance Num Complejo where -}


----------------------------------


--2a) Definir un tipo enumerado Direccion con cuatro valores que representen movimientos (arriba,abajo, izquierda, derecha) por una cuadŕıcula en el plano con coordenadas enteras. Convertirlo en instancia de Eq, Ord, Show usando deriving.

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Eq, Ord,Show) 
type Punto = (Integer,Integer)
--2b) Definir una funcion mueve movs punto que al aplicarse a un punto del plano y una lista demovimientos, devuelva el punto final al que se llega.
mueve :: [Direccion] -> Punto -> Punto
mueve [] (x,y) = (x,y)
mueve (z : zs) (x,y) 
                    |  z == Arriba     = mueve zs (x, y-1)
                    |  z == Abajo      = mueve zs (x, y+1)
                    |  z == Izquierda  = mueve zs (x-1, y)
                    |  otherwise       = mueve zs (x+1, y)

mueve' :: [Direccion] -> Punto -> Punto
mueve' zs (x0,y0) = foldl (\point direction -> f point direction ) (x0,y0) zs where f (x,y) dir
                                                                                            |  dir == Arriba     = (x, y-1)
                                                                                            |  dir == Abajo      = (x, y+1)
                                                                                            |  dir == Izquierda  = (x-1, y)
                                                                                            |  otherwise       = (x+1, y)


--2c)Definir  una  funcion trayectoria movs punto que  al  aplicarse  a  un  punto  del  plano  y  una lista de movimientos, devuelva la lista de puntos por los que se pasa al aplicar movs a punto.


trayectoria :: [Direccion] -> Punto -> [Punto]
trayectoria [] (x,y) = [(x,y)]
trayectoria (z:zs) (x,y) = (a,b) : (trayectoria zs (a,b))
                        where (a,b)  = (mueve [z] (x,y)) 

trayectoria' :: [Direccion] -> Punto -> [Punto]
trayectoria' zs (x0,y0) = foldl (\acc direction -> acc ++ [f (last acc) direction]) [(x0,y0)] zs where f (x,y) dir
                                                                                                            |  dir == Arriba     = (x, y-1)
                                                                                                            |  dir == Abajo      = (x, y+1)
                                                                                                            |  dir == Izquierda  = (x-1, y)
                                                                                                            |  otherwise       = (x+1, y)


--2d)  Definir una funcion inferior movs movs’ que devuelva True si la trayectoria determinada por movs a partir de cualquier punto nunca sube por encima de la determinada pormovs’.

inferior :: [Direccion] -> [Direccion] -> Bool
inferior xs ys = all (>= maximum (trayectoria xs (0,0))) (trayectoria ys(0,0))


--3) Definir un tipo de datos polimorfico para representar  ́arboles generales, en los que cada nodo tiene una informacion y n hijos (n≥0, y puede variar con cada nodo). 
--No se consideran  ́arboles vacıos.

data NArbol a = Vacio |Nodo a [NArbol a] deriving Eq


--a)
es_hoja:: NArbol Integer -> Bool
es_hoja (Nodo x [])                   = True
es_hoja (Nodo x xs)                   = False

lista_hojas:: NArbol Integer -> [Integer]
lista_hojas (Nodo x (y:ys))           
    | es_hoja (Nodo x (y:ys))         = x : lista_hojas (Nodo x ys)
    | otherwise                       = lista_hojas (Nodo x ys)
--b)
lista_nodos:: NArbol Integer -> [Integer]
lista_nodos (Nodo x (y:ys))            = x : lista_nodos y
lista_nodos (Nodo x [])                = [x]
--c)
instance Ord a => Ord (NArbol a) where 
	compare (Nodo x (a:as)) (Nodo y (b:bs))   = compare x y
--d)
instance Show a => Show (NArbol a) where
	show (Nodo x (as))   = show x ++ " childs(" ++ show (length as) ++") : \n" ++ show (head as)

--6) Considerese el siguiente tipo de datos para representar conjuntos finitos de elementos de un tipo cualquiera:data Conjunto a = Con Int [a]
--donde en un dato Con n xs que represente a un conjunto C, el argumento n representa el cardinal de C y xs es la lista de sus n elementos.
data Conjunto a = Con Int [a]

--6a)Definir una funcion to_con que convierta listas en conjuntos
--nub para elminar duplicados
nub:: (Eq a) => [a] -> [a]
nub l           = nub' l []
    where 
        nub' [] _       = []
        nub' (x:xs) ls  
            | elem x xs     = nub' xs ls 
            | otherwise     = nub' xs (x:ls)

myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (x:xs) = x : filter (/= x) (myNub xs)

myNoop :: Eq a=>[a] -> [a]
myNoop lista = foldr (\ x xs -> if elem x xs then xs else x : xs) [] lista

to_con:: [Int] -> Conjunto Int
to_con xs       = Con (length ys) ys
    where ys = nub xs
--6b) Definir la interseccion y la union de conjuntos.
--6c)Definir  la  funci ́onmapset f c,  que  calcula  la  imagen  porfde  un  conjuntoc,  es  decir,  elconjunto resultado de aplicar la funci ́onfa cada elemento dec.
--6d) Declarar Conjunto a como instancia de las clase Eq y Ord, de modo que == y <= reflejen la nocion matematica de igualdad de conjuntos y de inclusi ́on de conjuntos, respectivamente.


--a)


--b)
interseccion:: Conjunto Int -> Conjunto Int -> Conjunto Int 
interseccion (Con x (as)) (Con y (bs)) = to_con (nub [z | z <- as, elem z bs])
--c)
