sinHelper :: Double -> Int -> Int -> Double -> Double -> Double
sinHelper x n i prev acc
    | i == n-1  = acc
    | otherwise = sinHelper x n (i+1) term (acc+term)
    where denominator = fromIntegral ((i*2 + 2) * (i*2 + 3))
          updater = (-1) * x * x / denominator
          term = prev * updater

sin' :: Double -> Int -> Double
sin' x n = sinHelper x n 0 x x
