module Commandos where
import Control.Monad 
import System.Random
import System.Time
import Data.List

-- Part 1
data Command a = Command String (a -> IO a) 

constant :: String -> a -> Command a
constant s a = func s (const a)

func :: String -> (a -> a) -> Command a
func s f = iofunc s (return . f)

iofunc :: String -> (a -> IO a) -> Command a
iofunc = Command

iofunc_ :: String -> (a -> IO ()) -> Command a
iofunc_ s f = Command s (\x -> do f x ; return x)

example :: [Command Int]
example = 	[constant "zero" 0
			,func "inc" (+1)
			,func "dec" (subtract 1)
			,iofunc_ "print" (putStrLn . (">>> " ++ ) .show)]

-- Part 2
execute :: [Command a] -> a -> String -> IO a
execute (Command s f : xs) o c |c == s = f o
								|otherwise = execute xs o c
execute [] a _ = return a

runOnce :: Show a => [Command a] -> a -> IO a
runOnce xs a = do
		c <- getLine
		result <- execute xs a c
		return result

repeatM :: Monad m => (a -> m a) -> a -> m ()
repeatM f a = do
			a' <- f a
			repeatM f a'
 

run :: Show a => [Command a] -> a -> IO ()
run cs = repeatM (runOnce cs)

-- Part 3
showTimeDiff :: TimeDiff -> String
showTimeDiff td = "Elapsed time: "  ++ show (tdMin td) ++ ":" ++ show (tdSec td)

stopwatch :: [Command ClockTime]
stopwatch = [ iofunc "start" (const getClockTime)
			, iofunc_ "cur" (\a -> do x <- getClockTime ; putStrLn (showTimeDiff $ diffClockTimes x a))
			]