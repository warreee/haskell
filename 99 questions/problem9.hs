packtosub :: (Eq a) => [a] -> [[a]]
packtosub [] = []
packtosub [x] = [[x]]
packtosub (x:xs) 
    | x `elem` (head (packtosub xs)) = (x:(head (packtosub xs))) : (tail (packtosub xs))
    | otherwise                      = [x]:(packtosub xs)
