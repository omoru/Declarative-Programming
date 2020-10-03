
{-zip33 :: [a] -> [a] -> [a] -> [(a,a,a)]
zip33 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip33 xs ys zs
zip33 [] ys zs = []
zip33 xs [] zs = []
zip33 xs ys [] = []
-}
{-# LANGUAGE ParallelListComp #-}
zip33 :: [a] -> [a] -> [a] -> [(a,a,a)]
zip33 xs ys zs = [(x,y,z) | x <- xs
                          | y <- ys
                          | z <- zs]

imparesEn:: Integral a => [a] -> [a]
imparesEn []                = []
imparesEn xs             	= filter odd xs

escalar :: (Num a) => [a] -> [a] -> a
escalar xs ys = sum [ (fst x * snd x)  | x <- zip xs ys]


---- 6

--a) [(0,0),(1,2),(3,6),(7,14),(15,30),...

dobles = [ (x,2 * x) | x <- (map (\x -> (2 ^ x) - 1) [0..10]) ] 

--b) [1,-2,3,-4,5,-6,...]

negada = [if x `mod` 2 == 1 then x else negate x | x <-[ 1..100]]

--c)paresHasta n= lista de los n ́umeros naturales pares menores o iguales que n

paresHasta:: Integral a => a -> [a]
paresHasta n = [ x | x <- [1..n] ,not (odd x)]


-- d) lstPares n
lstPares:: Integral a => a -> [a]
lstPares n	= [x | x <- [1..2*n], odd x]

--e) mezclaParImpar xs ys= lista de todos los los pares posibles(x,y)tales que
--x espar y esta en la listaxs,yes impar y est ́a en la lista ys.

mezclaParImpar :: (Integral a) => [a] -> [a] -> [(a,a)]
mezclaParImpar xs ys= [(x,y) | x <- xs, y <- ys , odd y, not (odd x)]

-- prefijos xs

prefijos :: Integral a => [a] -> [[a]]
prefijos [] = []
prefijos (x : []) = [[x]]
prefijos xs =  [prefix | prefix <-  prefijos (init xs)]