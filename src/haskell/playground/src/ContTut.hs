{-# LANGUAGE RankNTypes #-}

module ContTut where

import Control.Monad.Cont

-- newtype Cont r a = Cont {runCont :: (a -> r) -> r}

-- callCC :: ((a -> Cont r a) -> Cont r a) -> Cont r a
-- https://www.williamyaoh.com/posts/2022-05-02-the-cont-monad.html

-- toCPS :: a -> (a -> r) -> r
-- toCPS x f = f x

-- fromCPS :: (forall r. (a -> r) -> r) -> a
-- fromCPS f = f id

-- -- Define a value of type (a -> r) -> r
-- cpsValue :: (Int -> r) -> r
-- cpsValue f = f 42

-- -- Use fromCPS to convert cpsValue back to an Int
-- intValue :: Int
-- intValue = fromCPS cpsValue

-- instance Functor (Cont r) where
--   fmap f (Cont cont) = Cont $ \cc -> cont (cc . f)

-- instance Applicative (Cont r) where
--   pure a = Cont $ \c -> c a

--   (Cont contFx) <*> (Cont contX) = Cont $ \c -> contFx $ \d -> contX $ \e -> c $ d e

-- instance Monad (Cont r) where
--   (Cont hof) >>= fma = Cont $ \bToR -> hof $ \a -> runCont (fma a) bToR

-- From: https://www.schoolofhaskell.com/user/dpiponi/the-mother-of-all-monads

-- ex1 :: Cont Int Int
ex1 = do
  let a =  1
  b <- cont $ \fred -> "escape"
  return $ a + b

test1 = runCont ex1 show

main :: IO ()
main = putStrLn test1
