import Data.Char
import Numeric

showUsingBase :: Int -> Int -> String
showUsingBase base num = go num ""
  where
    go num = case num `divMod` base of
        (0, r) -> showChar (intToDigit r)
        (d, r) -> go d . showChar (intToDigit r)
