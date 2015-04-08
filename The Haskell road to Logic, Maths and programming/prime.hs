
prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a positive integer"
	 | n == 1 = False
	 | otherwise = ld n == n

ld :: Integer -> Integer
ld n = ldf 2 n
ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
	| k^2 > n = n
	| otherwise = ldf (k+1) n

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs) | p x = x : filter2 p xs
				| otherwise = filter2 p xs

primes :: [Integer]
primes = filter2 prime0 [2..]

ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n 	| rem n p == 0 = p
				| p^2 > n = n
				| otherwise = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]
prime :: Integer -> Bool
prime n | n < 1 = error "not a positive integer"
		| n == 1 = False
		| otherwise = ldp n == n