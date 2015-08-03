
slice :: [a] -> Int -> Int -> [a]
slice xs k l = take (l - k + 1) $ drop (k-1) xs
