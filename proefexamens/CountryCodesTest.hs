module Main where

import CountryCodes


import Control.Exception (catch, SomeException(..))
import Control.Monad (liftM2)
import Data.Monoid
import Prelude hiding (catch) -- Required for older versions of GHC


main :: IO ()
main
  =  startTests
     <> test "1.1.1" (decodeRow 69 == "ACG")
     <> test "1.1.2" (decodeRow 240935 == "ABCFIKLNPQR")

     <> test "1.2.1" (take 10 (decode encodedCountryCodes)
                      == ["AD","AE","AF","AG","AI","AL","AM","AO","AQ","AR"])
     <> test "1.2.2" (decode [121, 0, 69]
                      == ["AA","AD","AE","AF","AG","CA","CC","CG"])

     <> test "2.1.1" (fillInGaps ["Alpha", "Delta"] ['A'..'E'] head (\c -> c : " ontbreekt")
                      == ["Alpha","B ontbreekt","C ontbreekt","Delta","E ontbreekt"])
     <> test "2.1.2" (fillInGaps [1 :: Int,5,9] [1..10] id id
                      == [1,2,3,4,5,6,7,8,9,10])
     <> test "2.1.3" (fillInGaps [134 :: Int, 578, 620, 834] [1..6] (`div` 100) (* 100)
                      == [134,200,300,400,578,620])

     <> test "2.2.1" (encodeChars "ACG" == 69)
     <> test "2.2.2" (encodeChars [] == 0)
     <> test "2.2.3" (encodeChars "XZ" == 41943040)

     <> test "2.3.1" (encodeRow ["AA","AC","AG"] == 69)
     <> test "2.3.2" (encodeRow ["UA","UC","UG"] == 69)

     <> test "2.4.1" (encode ["AA","ZZ"]
                      == [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33554432])
     <> test "2.4.2" (encode (decode encodedCountryCodes)
                      == encodedCountryCodes)
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
