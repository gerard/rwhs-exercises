listMean :: (Integral a, Fractional b) => [a] -> b
listMean xs = fromIntegral (foldr (+) 0 xs) / fromIntegral (length xs)
