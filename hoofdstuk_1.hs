module Ch_1

where

-- Geef het minimum van een lijst terug

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- Geef het maximum van een lijst terug

maxInt :: [Int] -> Int
maxInt [] = error "empty list" 
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- Verwijder het eerste voorkomen van getal x

removeFst :: Eq a => a -> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) 	| x == y = ys
					| otherwise = y : (removeFst x ys)

-- Sorteer een lijst

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- Sorteer een lijst met gebruik van let

srtInts2 :: [Int] -> [Int]
srtInts2 [] = []
srtInts2 xs = let
				m = mnmInt xs
				in m : (srtInts2 (removeFst m xs))

-- Het gemiddelde van een lijst

average :: [Int] -> Float
average [] = error "empty list"
-- average xs = fromInt (sum xs) / fromInt (length xs)

-- Oef 1.13 tel het aantal voorkomen van een karakter in een string

count :: Char -> String -> Int
count c [] = 0
count c (x:xs) 	| c==x = 1 + (count c xs)
				| otherwise = (count c xs)

copy :: Int -> Char -> String
copy 0 c = []
copy n c = c:(copy (n-1) c)

blowup :: String -> String
blowup xs = blowup' xs 1

blowup' :: String -> Int -> String
blowup' [] n = []
blowup' (x:xs) n = (copy n x) ++ (blowup' xs (n+1))


a :: [Int]
a = [5, 3, 1, 2, 4, 2]


prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys


substring :: String -> String -> Bool
substring [] ys = True
substring (x:xs) [] = False
substring (x:xs) (y:ys) = ((x==y) && (prefix xs ys))
						|| (substring (x:xs) ys)