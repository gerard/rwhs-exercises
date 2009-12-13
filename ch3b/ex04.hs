doPalin :: [a] -> [a]
doPalin (x:xs) = (x:doPalin xs) ++ [x]
doPalin []     = []
