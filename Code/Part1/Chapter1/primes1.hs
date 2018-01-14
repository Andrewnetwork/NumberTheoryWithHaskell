-- Integer Square Root
intSqrt :: (Integral a) => a -> Integer
intSqrt x =  round (sqrt (fromIntegral x))

-- Divisor Tuples
dt :: Integer -> [(Integer, Integer)]
dt x = [ (quot x i,i) 
        | i <- [k,k-1..1], mod x i == 0]
        where k = intSqrt(x)

-- Prime Factors 
pfs :: Integer -> [Integer]
pfs x
    | elem 1 z = [x]
    | otherwise = pfs (fst z) ++ pfs (snd z)
    where z = head (dt x)
