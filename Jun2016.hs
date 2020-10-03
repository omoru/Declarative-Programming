
--1)a)
f1a:: [(Integer,Integer)]
f1a = zip xs (map (\x -> 2^x) xs)
            where xs = [0..]
f1a' = [(x,2^x) | x<- [0..]]

--1)b) f x y = x (y x)

{-

f:: A -> B -> C
 x:: A
 y:: B
 x(yx)::C
 
(yx):: D
y:: B = A -> D = (D -> C) -> D 
x:: A = D -> C

(D -> C) -> ((D -> C) -> D) -> C (a -> b) -> ((a -> b) -> a) -> b

-}

f :: (t1 -> t2) -> ((t1 -> t2) -> t1) -> t2
f x y = x (y x)


--3) Define la funcion reverse con foldr y foldl

reverseR:: [a]->[a]
reverseR xs = foldr (\x y -> y ++ [x] ) [] xs

reverseL:: [a] -> [a]
reverseL xs = foldl (\x y -> y : x ) [] xs

reverse2:: Eq a => [a] -> [a]
reverse2 = foldl (flip (:)) []

fexam x 0 z = x == z
fexam x y z = x

fa x  False  = True
fa  False      y  = True
fa   True   True  = False

faa 0 y = gaa y
faa x y = haa y
gaa x = if x > 0 then 1 else 0
haa x = 0

h1 f g x y = f (g x y) x

data Arbol a b = Hoja a|Nodo b [(Arbol a b)]  deriving Show

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)