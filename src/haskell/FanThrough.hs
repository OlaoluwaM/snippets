-- This comes from base
import Control.Arrow (Arrow, (&&&))

fanThrough :: (Arrow a) => (a b c, a b c') -> a b (c, c')
fanThrough = uncurry (&&&)
