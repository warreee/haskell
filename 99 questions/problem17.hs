
split :: [a] -> Int -> [[a]]
split xs n = take n xs : [drop n xs]
