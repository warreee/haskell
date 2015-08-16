import Control.Monad
import Data.List

isPrime :: Int -> Bool
isPrime n = length ([ x | x <- [1..n], n `mod` x == 0]) == 2

goldbach :: Int -> [(Int,Int)]
goldbach n = [(x,y)| x <- [1..n], y <- map (n-) [1..n] , isPrime x, isPrime y, x + y == n]

goldbachM :: Int -> [(Int,Int)]
goldbachM n = do
        x <- [1..n]
        y <- [1..n `div` 2] \\ [x]
        guard (x + y == n)
        guard (isPrime x)
        guard (isPrime y)
        return (x,y)
           
        


