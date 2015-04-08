{-
1 Problem 1
(*) Find the last element of a list.
-}
-- The buildin function in haskell

myLast2 :: [a] -> a
myLast2 = last


-- The most obvious implemetation

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs
 
myLast' = foldr1 (const id)
 
-- Prelude> const 1 2
-- 1
-- Prelude> (flip const) 1 2
-- 2
myLast'' = foldr1 (flip const)
 
myLast''' = head . reverse
 
-- curry makes a curried function
myLast'''' = foldl1 (curry snd)
 
myLast''''' [] = error "No end for empty lists!"  
myLast''''' x = x !! (length x -1)
