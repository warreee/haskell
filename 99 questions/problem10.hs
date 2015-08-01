packtosub :: (Eq a) => [a] -> [[a]]
packtosub [] = []
packtosub [x] = [[x]]
packtosub (x:xs) 
    | x `elem` (head (packtosub xs)) = (x:(head (packtosub xs))) : (tail (packtosub xs))
    | otherwise                      = [x]:(packtosub xs)



encode :: (Eq a) => [a] -> [(Int,a)]
encode = encode' . packtosub 

encode' :: (Eq a) => [[a]] -> [(Int,a)]
encode' [] = []
encode' [x] = [((length x), (head x))]
encode' (x:xs) = ((length x), (head x)):(encode' xs)
