isPrime :: (Integral a) => a -> Bool
isPrime k = length [ x | x <- [2..k], k `mod` x == 0] == 1
