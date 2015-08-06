data Enco a = Single a | Multiple Int a deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [Enco a]
encode = encode' . pack

encode' :: (Eq a) => [[a]] -> [Enco a]
encode' [] = []
encode' (x:xs)  
    | length x == 1 = (Single (head x)) : (encode' xs)
    | otherwise     = (Multiple (length x) (head x)):(encode' xs)
