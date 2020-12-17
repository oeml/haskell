import Primes


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


mapHelper :: (a -> b) -> [a] -> [b] -> [b]
mapHelper _ [] ready     = ready
mapHelper f (x:xs) ready = mapHelper f xs (ready ++ [f x])

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = mapHelper f xs [f x]


boolean :: [a] -> [[a]]
boolean []     = [[]]
boolean (x:xs) = let ys = boolean xs in ys ++ (map (x:) ys)


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = let rest = filter' f xs in if f x then x:rest else rest


foldlX :: (b -> a -> b) -> b -> [a] -> b
foldlX _ z []     = z
foldlX f z (x:xs) = foldlX f (f z x) xs  -- tail recursion


foldrX :: (a -> b -> b) -> b -> [a] -> b
foldrX _ z []     = z
foldrX f z (x:xs) = f x (foldrX f z xs)


down 0 = Nothing
down x = Just (x-1,x)

binary 0 = Nothing
binary x = Just (x `div` 2, x `mod` 2)

factor 1 = Nothing
factor n = Just (fst (firstFactor n))
    where divByPrimes n = map (\p -> ((div n p, p), mod n p)) (takeWhile (<=n) primes)
          firstFactor n = head $ filter ((==0) . snd) (divByPrimes n)


unfold' :: (a -> Maybe (a,b)) -> a -> [b]
unfold' f x = maybe [] (\(u,v) -> v:(unfold' f u)) (f x)


data List' a = Nil
             | Cons a (List' a) deriving (Show)

mapL :: (a -> b) -> List' a -> List' b
mapL _ Nil         = Nil
mapL f (Cons x xs) = Cons (f x) (mapL f xs)
