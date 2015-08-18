import Control.Monad
import Control.Monad.State

data MayFail e a = Error e | Result a deriving (Show, Eq)

safeDiv :: Int -> Int -> MayFail String Int
safeDiv a b
    | b == 0 = Error "Division by zero"
    | otherwise = Result (div a b)
    
instance Functor (MayFail e) where
    fmap _ (Error e) = Error e
    fmap f (Result a) = Result (f a)
    
instance Monad (MayFail e) where
    return = Result
    Error e >>= _ = Error e
    Result a >>= f = f a  
    
data Exp = Lit Int | Add Exp Exp | Mul Exp Exp -- | Div Exp Exp

eval :: Exp -> MayFail String Int
eval (Lit a) = return a
eval (Add a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    return (x + y)
                    
eval (Mul a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    return (x * y)                    
                    
{-- eval (Div a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    safeDiv x y
                    if y /= 0 then return(div x y) else (Error "Division by zero") --} 
                    
evalDo (Add a b) = do
            x <- eval a
            y <- eval b
            return (x + y)           
            
------------------------WRITER MONAD ----------------------------------

data Writer a = Writer a String deriving (Show)

instance Functor Writer where
    fmap f (Writer a log) = Writer (f a) log
    

        
evalTrace :: Exp -> (Int,String)
evalTrace (Lit a) = (a,"Lit\n")
evalTrace (Add a b) = (x+y,log) 
                    where   x = fst (evalTrace a)
                            y = fst (evalTrace b)
                            log =  "Add\n" ++ (snd (evalTrace a)) ++ (snd (evalTrace b))        
                            
instance Monad Writer where
    return x = Writer x ""
    (Writer a log) >>= f =
        let Writer x log' = f a
        in  Writer x (log ++ log')




trace :: String -> Writer ()
trace s = Writer () s

evalTraceM :: Exp -> Writer Int
evalTraceM (Lit x) = do
  trace "Lit\n"
  return x
evalTraceM (Add x y) = do
  trace "Add\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv+yv)
evalTraceM (Mul x y) = do
  trace "Mul\n"
  xv <- evalTraceM x
  yv <- evalTraceM y
  return (xv*yv)   
  
--------------- Implementation of Monadic Functions-----------
                                                  
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr (liftM2 (:)) $ return []

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f a = sequence (map f a)
zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f xs ys = sequence (zipWith f xs ys)
replicateM' :: Monad m => Int -> m a -> m [a] 
replicateM' n x   = sequence (replicate n x)

class RandomGen g where
    next :: g -> (Int, g)
    genRange :: g -> (Int, Int)

nextS :: RandomGen g => State g Int
nextS = do
    g <- get
    let (x, g') = next g
    put g'
    return x                         
    
randomList :: RandomGen g => Int -> State g [Int]
randomList n
    | n > 0
    = do x <- nextS
         xs <- randomList (n-1)
         return (x:xs)
    | otherwise
    = return []                            
