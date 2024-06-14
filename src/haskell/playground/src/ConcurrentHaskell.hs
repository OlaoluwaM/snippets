module ConcurrentHaskell where

import Control.Concurrent

-- -------------- Section 3.1 From the paper Concurrent Haskell ------------- --
type CVar a =
  ( MVar a -- Producer -> consumer
  , MVar () -- Consumer -> producer
  )

newCVar :: (Num a) => IO (CVar a)
newCVar = do
  dataVar <- newMVar 10
  ackVar <- newMVar ()
  return (dataVar, ackVar)

putCVar :: CVar a -> a -> IO ()
putCVar (dataVar, ackVar) val = do
  takeMVar ackVar
  putMVar dataVar val

getCVar :: CVar a -> IO a
getCVar (dataVar, ackVar) = do
  val <- takeMVar dataVar
  putMVar ackVar ()
  return val
