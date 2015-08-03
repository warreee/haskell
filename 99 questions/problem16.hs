import Data.List

dropNth :: [a] -> Int -> [a]
dropNth xs n = multipleTakes xs ([0..last] \\ [index1,index2..last])
    where last = length xs - 1
          index1 = n - 1
          index2 = 2 * n - 1

-- takes all elements of [a] with index from [Int]
multipleTakes :: [a] -> [Int] -> [a]
multipleTakes _ [] = []
multipleTakes [] _ = []
multipleTakes xs (n:ns) = xs !! n : multipleTakes xs ns


