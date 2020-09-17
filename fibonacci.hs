fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n-1) + fib' (n-2)

fibHelp :: Int -> Integer -> Integer -> Integer
fibHelp 0 a _ = a
fibHelp n a b = fibHelp (n-1) b (a+b)

fib n = fibHelp n 0 1


fibgen :: Integer -> Integer -> [Integer]
fibgen a b = a:(fibgen b (a+b))

fibs' = fibgen 0 1

fibs = 0:1:(zipWith (+) fibs (tail fibs))
