import System.Random

--randomNselect :: [a] -> Int -> [a]


-- Generates an array of n random elements
randomArray :: Int -> Int -> Int -> IO [Int]
randomArray n lo hi
    | n <= 0    = []
    | otherwise =  getStdRandom (randomR (lo,hi)) : randomArray (n - 1) lo hi


