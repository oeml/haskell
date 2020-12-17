data Point = Point Int Int deriving (Show)

movePoint :: Point -> Int -> Int -> Point
movePoint (Point x y) u v = Point (x+u) (y+v)
