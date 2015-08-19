-- VOORNAAM NAAM
-- R-NUMMER
-- RICHTING
module CountryCodes where

import Data.Bits (setBit, testBit)
import Data.Char (ord, chr)
import Data.Word (Word32)
import Control.Applicative

encodedCountryCodes :: [Word32]
encodedCountryCodes
  = [48191864, 57637883, 66223597, 33576464, 917716, 153344, 23050747, 1717248,
     1013784, 53264, 54702544, 20841735, 67108093, 34785653, 4096, 21904625, 1,
     5521424, 61767647, 40533740, 50597953, 1057109, 262176, 0, 524304, 4198401]


-- Opdracht 1
decodeRow :: Word32 -> [Char]
decodeRow w = [ chr (i + ord 'A')
              | i <- [0..26]
              , testBit w i ]
                                                           


decode :: [Word32] -> [String]
decode = byPair . concat . map decodeRow 

byPair [] = []
byPair (x:y:z) = [x,y] : (byPair z) 


-- Opdracht 2
fillInGaps :: Eq ref => [a] -> [ref] -> (a -> ref) -> (ref -> a) -> [a]
fillInGaps = undefined

encodeChars :: [Char] -> Word32
encodeChars = undefined

encodeRow :: [String] -> Word32
encodeRow = undefined

encode :: [String] -> [Word32]
encode = undefined


-- Opdracht 3
printCodes :: [String] -> IO ()
printCodes = undefined
