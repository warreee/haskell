
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate list@(x:xs) n 
    | n >= 0    = rotate (xs ++ [x]) (n-1)
    | otherwise = rotate (last list : init list) (n+1)
