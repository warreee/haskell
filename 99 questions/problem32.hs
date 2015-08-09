
gcd' :: (Integral a) => a -> a -> a
gcd' x y = if y == 0 then x else gcd y (x `mod` y)
