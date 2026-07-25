combineDecimalDigits :: [Int] -> Int
combineDecimalDigits [] = 0
combineDecimalDigits digits = sum $ zipWith (\digit numOfZeros -> digit * (10 ^ numOfZeros)) (reverse digits) [0 ..]
