import Data.List

-- n must be positive
combination :: (Eq a) => Int -> [a] -> [[a]]
combination n = nub . map (take n) . permutations


