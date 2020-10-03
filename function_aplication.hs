


($) :: (a -> b) -> a -> b  
f $ x = f x  

{-
Whereas normal function application (putting a space between two things) has a really high precedence,
 the $ function has the lowest precedence. Function application with a space is left-associative
(so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.

 sum (map sqrt [1..130]) = sum $ map sqrt [1..130]
 sqrt (3 + 4 + 9)        = sqrt $ 3 + 4 + 9

How about sum (filter (> 10) (map (*2) [2..10]))? Well, because $ is right-associative
, f (g (z x)) is equal to f $ g $ z x. And so, we can rewrite
 sum (filter (> 10) (map (*2) [2..10])) as sum $ filter (> 10) $ map (*2) [2..10]. 

ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
[7.0,30.0,9.0,1.7320508075688772]


 That's why you can imagine a $ being sort of the equivalent of writing an opening parentheses and
  then writing a closing one on the far right side of the expression.
-}

