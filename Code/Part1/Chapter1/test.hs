-- Largest divisor tuple 
ldt x
    | isPrime x = (x,1)
    | otherwise = snd (minimum (zip (sumTups dt) dt))
    where dt = divisorTuples x

divisors x = [ z 
                | z <- [x/i 
                | i <- [x-1,x-2..2]], isInt z]
genPrimes start end = [z | z <- [start..end], isPrime z ]


psr x = [z | z <- [1..x],z * z == x ]

srt x
    | ps /= [] = ps
    | otherwise = srt (x-1)
    where ps = psr x

-- Tuple to list
ttl t = [fst t, snd t]

-- Sum Tuple List
stl tups = [a + b | (a,b) <- tups]


-- Sum Divisor Tuples 
sumDTups x = stl (dt x)