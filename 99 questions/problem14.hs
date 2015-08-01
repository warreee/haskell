dupli :: [a] -> [a]
dupli [] = []
dupli [x] = x : [x]
dupli (x:xs) = dupli [x] ++ dupli xs
