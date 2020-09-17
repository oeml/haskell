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


down 0 = Nothing
down x = Just (x-1,x)

unfold' :: (a -> Maybe (a,b)) -> a -> [b]
unfold' f x = maybe [] (\(u,v) -> v:(unfold' f u)) (f x)


data List' a = Nil
             | Cons a (List' a) deriving (Show)

mapL :: (a -> b) -> List' a -> List' b
mapL _ Nil         = Nil
mapL f (Cons x xs) = Cons (f x) (mapL f xs)
