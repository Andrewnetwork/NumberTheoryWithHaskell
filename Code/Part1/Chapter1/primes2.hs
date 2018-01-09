-- Is Integer 
isInt x = x == fromInteger (round x)

-- Is Divisor List 
isDivisors x = [ isInt z 
                    | z <- [x/i 
                    | i <- [2,3..x-1]] ]
-- Is Prime
isPrime x = not (elem True (isDivisors x))

-- Divisor Tuples
dt x
    | isPrime x = [(x,1)]
    | otherwise = [ z 
                    | z <- [(x/i,i) 
                    | i <- [2,3..sqrt(x)]], isInt (fst z)]

-- Greatest Factor Tuple 
gft :: (RealFrac a, Integral a, Floating a) => a -> [(a, a)]
gft x = take 1 [ z 
            | z <- [(x/i,i) 
            | i <- [k,k-1..1]], isInt (fst z)]
        where k = floor(sqrt(x))

-- Prime Factors 

