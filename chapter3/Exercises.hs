import           Data.List

myLength :: [a] -> Int
myLength (x : xs) = 1 + myLength xs
myLength []       = 0

myMean :: (Fractional a) => [a] -> a
myMean [] = 0
myMean xs = (mySum xs) / (fromIntegral $ myLength xs)
 where
  mySum []       = 0
  mySum (x : xs) = x + mySum xs

toPalindrome :: [a] -> [a]
toPalindrome []       = []
toPalindrome (x : xs) = x : toPalindrome xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs ++ xs == toPalindrome xs

sortLength :: [[a]] -> [[a]]
sortLength xs =
  let sortLength a b | myLength a > myLength b  = GT
                     | myLength a == myLength b = EQ
                     | otherwise                = LT
  in  sortBy sortLength xs

myIntersperse :: a -> [[a]] -> [a]
myIntersperse s []       = []
myIntersperse s [x     ] = x
myIntersperse s (x : xs) = x ++ s : (myIntersperse s xs)

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)

data Point = Point Double Double deriving (Show)

findDirection :: Point -> Point -> Point -> Direction
findDirection (Point x1 y1) (Point x2 y2) (Point x3 y3) | zCoord > 0 = LeftTurn
                                                        | zCoord == 0 = Straight
                                                        | otherwise = RightTurn
  where zCoord = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

findDirections :: [Point] -> [Direction]
findDirections (x : y : z : ws) =
  findDirection x y z : findDirections (y : z : ws)
findDirections _ = []

grahamOrder :: [Point] -> [Point]
grahamOrder xs = p0 : (orderByCos . tail $ p xs)
 where
  minY (Point x1 y1) (Point x2 y2) | y1 > y2             = GT
                                   | y1 == y2 && x1 > x2 = GT
                                   | otherwise           = LT
  p  = sortBy minY
  p0 = head $ p xs
  cos2 (Point x1 y1) (Point x2 y2) =
    (x2 - x1) / sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) -- a /  c = cos beta 
  cos2_0 = cos2 p0
  orderCos p1 p2 | cos2_0 p1 > cos2_0 p2  = LT
                 | cos2_0 p1 == cos2_0 p2 = minY p1 p2
                 | otherwise              = GT
  orderByCos = sortBy orderCos

-- return in reverse order      
graham :: [Point] -> [Point]
-- should clean from repeated points
graham = operate [] . grahamOrder
 where
  operate stack []       = stack
  operate stack (y : ys) = operate (iter stack y) ys
  back (x : y : z : zs) next
    | findDirection z y next == LeftTurn = next : y : z : zs
    | otherwise                          = back (y : z : zs) next
  back (x : y : []) next = [next, y]
  iter (x : y : ys) next
    | findDirection y x next == LeftTurn = next : x : y : ys
    | otherwise                          = back (x : y : ys) next
  iter xs next = next : xs

grahamPrettier :: [Point] -> [Point]
grahamPrettier = foldl iter []
 where
  back (x : y : z : zs) next
    | findDirection z y next == LeftTurn = next : y : z : zs
    | otherwise                          = back (y : z : zs) next
  back (x : y : []) next = [next, y]
  iter (x : y : ys) next
    | findDirection y x next == LeftTurn = next : x : y : ys
    | otherwise                          = back (x : y : ys) next
  iter xs next = next : xs
