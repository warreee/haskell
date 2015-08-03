repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc x -> acc ++ replicate n x) [] xs
