intersperse :: a -> [[a]] -> [a]
intersperse c (x:xs)
    | null xs   = x
    | otherwise = x ++ [c] ++ (intersperse c xs)
