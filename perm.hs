permutate :: (Eq a) => [a] -> [[a]]
permutate [] = [[]]
permutate xs = [x:rest | x <- xs, rest <- permutate $ filter (\a -> a /= x) xs]
