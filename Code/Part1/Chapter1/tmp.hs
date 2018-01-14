-- Is Integer 
isInt x = x == fromInteger (round x)

intSqrt x = fromInteger (round (sqrt x))

-- Is Divisor List 
isDivisors x = [ isInt z 
                    | z <- [x/i 
                    | i <- [2,3..intSqrt(x)]] ]
-- Is Prime
isPrime x = not (elem True (isDivisors x))

-- Divisor Tuples
dt x = [ z 
         | z <- [(x/i,i) 
         | i <- [k,k-1..1]], isInt (fst z)]
         where k = intSqrt(x)

-- Greatest Factor Tuple 
gft x = head (dt x)

-- Prime Factors 
pfs x
    | elem 1 z = [x]
    | otherwise = pfs (fst z) ++ pfs (snd z)
    where z = gft x

