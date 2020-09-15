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
concat' xs []     = xs
concat' xs (y:ys) = concat' (xs++[y]) ys


concat'' :: [[a]] -> [a]
concat'' []       = []
concat'' [x]      = x
concat'' (x:y:xs) = concat'' ((concat' x y):xs)


fmapHelper :: (a -> b) -> [a] -> [b] -> [b]
fmapHelper _ [] ready   = ready
fmapHelper f (x:xs) ready = fmapHelper f xs (ready ++ [f x])

fmap' :: (a -> b) -> [a] -> [b]
fmap' f (x:xs) = fmapHelper f xs [f x]
