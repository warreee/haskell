import Control.Monad

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
    
data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Div Exp Exp

eval :: Exp -> MayFail String Int
eval (Lit a) = return a
eval (Add a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    return (x + y)
                    
eval (Mul a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    return (x * y)                    
                    
eval (Div a b) =    eval a >>= \x -> 
                    eval b >>= \y -> 
                    safeDiv x y
                    --if y /= 0 then return(div x y) else (Error "Division by zero")  
                    
evalDo (Add a b) = do
            x <- eval a
            y <- eval b
            return (x + y)                                         
