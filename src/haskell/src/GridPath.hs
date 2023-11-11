module GridPath where

  gridPath :: Integer -> Integer -> Integer
  gridPath 1 _ = 1
  gridPath _ 1 = 1
  gridPath n m = gridPath (n - 1) m + gridPath n (m - 1)
