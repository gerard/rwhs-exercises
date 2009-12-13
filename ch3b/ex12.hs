import Data.List

type Point        = (Integer, Integer)
type Vector       = (Integer, Integer)
data Direction    = GoesStraight | GoesLeft | GoesRight deriving (Show, Eq)

testData :: [Point]
testData  = [(1,4), (1,1), (3,5), (3,4), (3,2), (2,3), (3,1), (5,4), (5,5)]

toVector :: Point -> Point -> Vector
toVector (ax, ay) (bx, by) = (bx - ax, by - ay)

getDirFromV :: Vector -> Vector -> Direction
getDirFromV (vx, vy) (wx, wy)
    | xprod == 0 = GoesStraight
    | xprod  > 0 = GoesLeft
    | xprod  < 0 = GoesRight
    where xprod = vx*wy - vy*wx

getDirFromP :: Point -> Point -> Point -> Direction
getDirFromP a b c = getDirFromV (toVector a b) (toVector b c)

getVDirs :: [Point] -> [Direction]
getVDirs (a:b:c:s) = (getDirFromP a b c):(getVDirs (b:c:s))
getVDirs _         = []

findStartPoint :: [Point] -> Point
findStartPoint ps = foldr betterStartPoint (last ps) ps
    where betterStartPoint (px, py) (qx, qy)
            | py < qy   = (px, py)
            | py > qy   = (qx, qy)
            | px < qx   = (px, py)
            | px > qx   = (qx, qy)
            | otherwise = (px, py)

cotanToX :: Point -> Point -> Double
cotanToX (ax, ay) (bx, by) = dy/dx
    where dy = (fromIntegral (by - ay))
          dx = (fromIntegral (bx - ax))

sortFun :: Point -> Point -> Point -> Ordering
sortFun p a b
    | cotanToX p a > cotanToX p b = GT
    | otherwise                   = LT

sortPoints :: [Point] -> [Point]
sortPoints ps = sortBy (sortFun (findStartPoint ps)) ps

getGrahamPath :: [Point] -> [Point]
getGrahamPath ps = [p] ++ (filter (/= p) (sortPoints ps)) ++ [p]
    where p = findStartPoint ps

isLeftTurning :: (Direction, Point) -> Bool
isLeftTurning (d, _) = d == GoesLeft

getAnnotatedPath :: [Point] -> [(Direction, Point)]
getAnnotatedPath ps = zip (getVDirs (gpath)) (tail gpath)
    where gpath = getGrahamPath ps

getConvexHullWithDir :: [Point] -> [(Direction, Point)]
getConvexHullWithDir ps = first:(filter isLeftTurning (getAnnotatedPath ps))
    where first = (GoesLeft, findStartPoint ps)

removeDirection :: [(Direction, Point)] -> [Point]
removeDirection ((_, p):ps) = p:(removeDirection ps)
removeDirection []          = []

getConvexHull :: [Point] -> [Point]
getConvexHull = removeDirection . getConvexHullWithDir
