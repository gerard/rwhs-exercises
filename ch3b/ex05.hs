isPalin :: (Eq a) => [a] -> Bool
isPalin (x:xs) 
    | xs == []      = True
    | x  /= last xs = False
    | x  == last xs = isPalin (init xs)
isPalin [] = True
