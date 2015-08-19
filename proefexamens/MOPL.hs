module MOPL where
--Exercise 1
data Statement = Assign String Term | Print Term
data Term =  VarTerm String | IntTerm Int | Opt Term Term | Aft Term Term | Ver Term Term

--Exercise 2
assign :: String -> Term -> Statement
assign str term = Assign str term

printTerm :: Term -> Statement
printTerm = Print

intTerm :: Int -> Term
intTerm = IntTerm

varTerm :: String -> Term 
varTerm = VarTerm

plus :: Term -> Term -> Term
plus = Opt

times :: Term -> Term -> Term
times = Ver

minus :: Term -> Term -> Term
minus = Aft

--Exercise 3
type State = [(String,Int)]

valueOf :: State -> String -> Int
valueOf (x:xs) str
        | fst x == str = snd x
        | otherwise = valueOf xs str 

insertS :: String -> Int -> State -> State
insertS str i [] = [(str, i)]
insertS str i (x:xs) 
        | fst x == str  = (str,i):xs
        | otherwise     = x : insertS str i xs


--Exercise 4
evalTerm :: State -> Term -> Int 
evalTerm state (IntTerm a) = a
evalTerm state (VarTerm a) = valueOf state a
evalTerm state (Ver a b) = (evalTerm state a) * (evalTerm state b)
evalTerm state (Aft a b) = (evalTerm state a) - (evalTerm state b)
evalTerm state (Opt a b) = (evalTerm state a) + (evalTerm state b)

--Exercise 5
execAssign :: String -> Term -> State -> State
execAssign str t st = insertS str (evalTerm st t) st 

--Exercise 6
type Program = [Statement]

execPure :: State -> Program -> State
execPure st [] = st
execPure st ((Print a):xs) = execPure st xs
execPure st ((Assign a b):xs) = execPure (execAssign a b st) xs
--Exercise 7

execute :: Program -> IO ()
execute [] = return ()
execute prg = execute2 [] prg 

execute2 :: State -> Program -> IO ()
execute2 _ [] = return ()
execute2 st ((Print x):xs) = do 
                    print (evalTerm st x)
                    execute2 st xs
execute2 st ((Assign a b): xs) = do
                    execute2 (execAssign a b st) xs                    
                    
