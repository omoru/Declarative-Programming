f' :: Int -> Int -> Int
f' x y
    | x > 0 = y
    | otherwise = x
f' 0 1 = 2

f :: Int -> Int -> Int
f 0 1 = 2
f x y
    | x > 0 = y
    | otherwise = x

ordena :: Ord a => a -> a -> a -> (a,a,a)
ordena x y z = if (x <= y && x <= z && y <= z)
               then (x,y,z)
               else if (y <= x && y <= z && x <= z)
                    then (y,x,z)
                    else if (x <= y && x <= z && z <= y)
                         then (x,z,y)
                         else (x,y,z) -- por no continuar

ordena2 :: Ord a => a -> a -> a -> (a,a,a)
ordena2 x y z = if (x <= y && x <= z && y <= z)
                    then (x,y,z)
                else if (y <= x && y <= z && x <= z)
                    then (y,x,z)
                else if (x <= y && x <= z && z <= y)
                    then (x,z,y)
                else (x,y,z)
simplifica:: Integral a => (a,a) -> (a,a)
simplifica (_,0)  			= error "no definido"
simplifica (x,y)			= (div x a, a) where a = mcd x y

mcd:: Integral a => a -> a -> a
mcd 0 0 				= error "no definido"
mcd x 0					= abs x
mcd x y 				= mcd b (mod a b)
	where a = abs x; b = abs y

data   T a = P | Q a (T a)deriving (Show, Eq, Ord)

faaaa x y z = if (\x -> y < x) z then x y else (\y -> y . (+2)) x z