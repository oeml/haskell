-- argument types
sin' :: Float -> Int -> Float
sin' x 0 = x
sin' x n = let oldterm = sin' x (n-1) in oldterm * (1 - x^2 / n) 
