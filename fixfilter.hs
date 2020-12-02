import Data.Function

filter' :: (a -> Bool) -> [a] -> [a]
filter' = fix (\ rec f xs -> 
    case xs of [] -> []
               (x:rest) -> let x = head xs
                               rest = rec f (tail xs)
                           in if f x then x:rest else rest)
