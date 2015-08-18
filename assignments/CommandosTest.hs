module Main where

import Commandos


import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM2)
import Data.Monoid
import System.IO.Unsafe

ex :: Board
ex = [[1,0,1,0],[4,8,4,0],[8,8,8,8],[0,0,0,0]]

sol :: Board
sol = [[0,0,0,2],[0,4,8,4],[0,0,16,16],[0,0,0,0]]

ex2 :: Board
ex2 = [[1,4,8,0],[0,8,8,0],[1,4,8,0],[0,0,8,0]]

sol2 :: Board
sol2 = [[0,0,0,0],[0,4,0,0],[0,8,16,0],[2,4,16,0]]

main :: IO ()
main
  =  startTests
     <> test "execute example 5 zero" (unsafePerformIO (execute example 5 "zero") == 0)
     <> test "execute example 0 inc" (unsafePerformIO (execute example 0 "inc") == 1)
     <> test "execute example 1 dec" (unsafePerformIO (execute example 1 "dec") == 0)
     <> test "execute example 0 oeps" (unsafePerformIO (execute example 0 "oeps") == 0)
     <> test "execute example 0 print" (unsafePerformIO (execute example 0 "print") == 0)
     <> test "emptyBoard" (emptyBoard == [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]])
     <> test "moveRight" (moveRight  ex == sol)
     <> test "moveLeft" (map reverse (moveLeft (map reverse ex)) == sol)
     <> test "moveUp" ((reverse . moveUp . reverse) ex2 == sol2)
     <> test "moveDown" (moveDown ex2 == sol2)
     <> test "don't add a random number when the board doesn't change" (unsafePerformIO (move Up emptyBoard) == emptyBoard)
     >>= endTests





-- Mini testing framework
test :: String -> Bool -> IO Results
test msg b
  = do notImplemented <- isUndefined b
       case notImplemented of
         True      -> printResult yellow "function not implemented" >> return (Sum 1, Sum 0, Sum 0)
         False | b -> printResult green "passed" >> return (Sum 0, Sum 0, Sum 1)
         _         -> printResult red "failed" >> return (Sum 0, Sum 1, Sum 0)
  where printResult colorCode result
          = putStrLn $ "Test " ++ msg ++ " " ++ colorise colorCode result

type Results = (Sum Int, Sum Int, Sum Int) -- (Not implemented, failed, passed)

instance Monoid a => Monoid (IO a) where
  mempty = return mempty
  mappend = liftM2 mappend

startTests :: IO Results
startTests = putStrLn "Testing your solutions" >> return (Sum 0, Sum 0, Sum 0)

endTests :: Results -> IO ()
endTests (notImpl, failed, passed)
  = case (getSum notImpl, getSum failed, getSum passed) of
     (0, 0, _) -> putStrLn $ colorise green "All tests passed"
     (n, f, p) -> putStrLn $ unwords $
                  filter (not . null) [nNotImpl n, nFailed f, nPassed p]
  where nPassed 0 = ""
        nPassed p = colorise green $ show p ++ " tests passed"
        nFailed 0 = ""
        nFailed f = colorise red $ show f ++ " tests failed"
        nNotImpl 0 = ""
        nNotImpl n = colorise yellow $ show n ++ "x function not implemented"

isUndefined :: a -> IO Bool
isUndefined a = (a `seq` return False) `catch` \(SomeException _) -> return True

red, green, yellow :: Int
(red, green, yellow) = (31, 32, 33)

colorise :: Int -> String -> String
colorise colorCode s = "\ESC[0;" ++ show colorCode ++ "m" ++ s ++ "\ESC[0m"
