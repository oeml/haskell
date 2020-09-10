lengthx :: [a] -> Int
lengthx []     = 0
lengthx (_:xs) = 1 + lengthx xs

at' :: [a] -> Int -> a
at' (x:xs) 0 = x
at' (x:xs) i = at' xs (i-1) 

reverseHelper :: [a] -> [a] -> [a]
reverseHelper []     ys = ys
reverseHelper (x:xs) ys = reverseHelper xs (x:ys)

reverse' :: [a] -> [a]
reverse' xs = reverseHelper xs []

concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x:(concat' xs ys)
-- the above is not tail recursion (I think); 
-- need to rewrite using tail recursion

