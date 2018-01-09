-- Is Integer 
isInt x = x == fromInteger (round x)

-- Is Divisor List 
isDivisors x = [ isInt z 
                    | z <- [x/i 
                    | i <- [2,3..sqrt(x)]] ]
-- Is Prime
isPrime x = not (elem True (isDivisors x))

-- Divisor Tuples
dt x = [ z 
         | z <- [(x/i,i) 
         | i <- [2,3..sqrt(x)]], isInt (fst z)]

-- Greatest Factor Tuple 
gft x
    | z == [] = (x,1)
    | otherwise = last z
    where z = dt x

-- Prime Factors 
pfs x
    | elem 1 z = [x]
    | otherwise = pfs (fst z) ++ pfs (snd z)
    where z = gft x

