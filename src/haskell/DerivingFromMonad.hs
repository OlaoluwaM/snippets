module DerivingFromMonad where

import Control.Monad (ap, liftM)

-- We can somewhat automatically derive both a Functor instance and an Applicative instance if an entity already has a Monad instance, since a Monad is the stronger of the three algebras
-- Playground link: https://play.haskell.org/saved/n2dHV4y0
data Eith e a = Lef e | Rig a deriving (Show, Eq, Ord)

instance Functor (Eith e) where
    fmap = liftM

instance Applicative (Eith e) where
    pure = Rig
    (<*>) = ap

instance Monad (Eith e) where
    (Rig a) >>= f = f a
    (Lef e) >>= f = Lef e

instance (e ~ String) => MonadFail (Eith e) where
    fail = Lef

foo :: Eith String Int
foo = fail "blah"

main :: IO ()
main = print foo
