type Point        = (Int, Int)
type Vector       = (Int, Int)
data Direction    = GoesStraight | GoesLeft | GoesRight deriving (Show)

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
