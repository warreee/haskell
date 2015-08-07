import Data.List

-- n must be positive
combination :: (Eq a) => Int -> [a] -> [[a]]
combination n = nub . map (take n) . permutations

combinations :: (Eq a) => [Int] -> [a] -> [[a]]
combinations xs ys = foldl (\acc x -> acc ++ combination x ys) [] xs  

comb :: (Eq a) => [Int] -> [a] -> [[a]]
comb xs ys = [ q | q <- combinations xs ys, nodoubles [q], (length . concat) [q] == sum xs]


nodoubles :: (Eq a) => [[a]] -> Bool
nodoubles xs = (length . concat) xs <= (length . nub . concat) xs
