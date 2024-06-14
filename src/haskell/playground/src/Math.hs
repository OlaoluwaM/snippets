module Math where

import PyF

phi :: Double
phi = (1 + sqrt 5) / 2

fibApprox :: Double -> Double
fibApprox n = (phi ** n) / sqrt 5

fib :: (Num a, Eq a) => a -> a
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  print [[fmt|x is {x}, fib of {x} is {fib x}, and fibApprox of {x} is {fibApprox x}|] | x <- [1 .. 5]]
