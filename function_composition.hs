(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x) 

{-


Mind the type declaration. f must take as its parameter a value that has the same type as g's 
return value. So the resulting function takes a parameter of the same type that g takes
 and returns a value of the same type that f returns.
  The expression negate . (* 3) returns a function that takes a number, multiplies it by 3
   and then negates it.
   
   -Con lambda :
   ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
   [-5,-3,-6,-7,-3,-2,-19,-24] 

   -Con composicion:
    ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
    [-5,-3,-6,-7,-3,-2,-19,-24]  



    f (g (z x)) is equivalent to (f . g . z) x

    Otro ejemplo :

    -Con lambda :
    ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
    [-14,-15,-27] 

    -Con composicion: 
    ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
    [-14,-15,-27] 
   -}

oddSquareSum' :: Integer
oddSquareSum' = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer
oddSquareSum'' =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit