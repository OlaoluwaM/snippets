module ZipListAsConsecutivePairs where

-- (tail l) just removes the head of a list, so [1, 2, 3] becomes [2, 3]
-- so if l = [1, 2, 3]
-- we get, zip [1, 2, 3] [2, 3]
-- which becomes [(1, 2), (2, 3)]
-- really useful
zipListAsConsecutivePairs :: [a] -> [a]
zipListAsConsecutivePairs l = zip l (tail l)

