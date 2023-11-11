module Test where

import Control.Concurrent.Chan

test1 :: IO ()
test1 = do
  chan <- newChan
  writeChan chan (10 :: Integer)
  val <- readChan chan
  print $ "Value: " ++ show val

test2 :: IO ()
test2 = do
  chan <- newChan
  anotherChan <- dupChan chan
  writeChan chan (10 :: Integer)
  valFromDupChan <- readChan anotherChan
  valFromOGChan <- readChan chan

  print $ "Value from dup chan: " ++ show valFromDupChan
  print $ "Value from OG chan: " ++ show valFromOGChan
