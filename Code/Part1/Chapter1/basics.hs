--flatten [] = []
--flatten [x] = x
--flatten (x:xs) = x : flatten xs 


natNums x
    | x == 0 = []
    | otherwise = x:natNums (x-1)
