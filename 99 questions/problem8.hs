rmDup :: (Eq a) => [a] -> [a]
rmDup [] = []
rmDup [x] = [x]
rmDup (x:y:zs) 
    | x == y    = rmDup (y:zs)
    | otherwise = [x] ++ rmDup (y:zs)
