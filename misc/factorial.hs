fact' :: (Integral a) => a -> a
fact' 0 = 1
fact' n = n * fact' (n-1)

factHelper :: Integer -> Integer -> Integer
factHelper 0 a = a
factHelper n a = factHelper (n-1) (a*n)

fact n = factHelper n 1 
