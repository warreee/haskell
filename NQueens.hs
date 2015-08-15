import Control.Monad 
import Data.List ((\\))


type Pos = [Integer]

x = [1..8]
y = [1..8]
u = [-7..7]
v = [2..16]

possiblePos :: Int -> [Queen]
possiblePos n = [(a,b) | a <- [1..n], b <- [1..n]]

type Queen = (Int,Int) 

--addQueen :: [Queen] -> Bool
--addQueen xs = or $ map (attacks (1,1)) xs

queens :: Int -> [[Int]]
queens n = foldM f [] [1..n]

queens'' = do
        a2 <- f [] 1
        a3 <- f a2 2
        a4 <- f a3 3
        a7 <- f a4 6
        a8 <- f a7 7
        f a8 8
        
  
f qs _ = [q:qs | q <- [1..8] \\ qs, q `notDiag` qs]
q `notDiag` qs = and [abs (q - qi) /= i | (qi,i) <- qs `zip` [1..]]
      
queens' :: Int -> [[Queen]]
queens' n = foldM g [] $ possiblePos n

g :: (Show a) => [Queen] -> a -> [[Queen]]
g qs _ = [q:qs | q <- (possiblePos 4) \\ qs, noAttacks q qs]


noAttacks :: Queen -> [Queen] -> Bool
noAttacks q [] = True
noAttacks (x,y) ((u,v):qs) 
        | x == u || y == v || x - y == u - v || x + y == u + v  = False
        | otherwise                                             = noAttacks (x,y) qs
        
test = [(a,b)| a <- [1..8], b <- [1..8] \\ [a]]        


        
            
            
           
-- prints what the board looks like for a solution; with an extra newline
printSolution y = do
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'Q' else '.' | z <- [1..n]]) y
     putStrLn ""
 
-- prints all the solutions for 6 queens
--main = mapM_ printSolution $ queens 6           
