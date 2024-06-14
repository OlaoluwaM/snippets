module Thread where

import Control.Concurrent

getGreeting :: IO String
getGreeting = do
  threadId <- myThreadId
  let greeting = "Hello from " ++ show threadId
  return $! greeting

threadHello :: Chan () -> IO ()
threadHello channelOfEndFlags = do
  greeting <- getGreeting
  putStrLn greeting
  writeChan channelOfEndFlags ()

main :: IO ()
main = do
  channelOfEndFlags <- newChan
  forkIO $ threadHello channelOfEndFlags
  forkIO $ threadHello channelOfEndFlags
  forkIO $ threadHello channelOfEndFlags
  forkIO $ threadHello channelOfEndFlags
  forkIO $ threadHello channelOfEndFlags
  mapM_ (\_ -> readChan channelOfEndFlags) [1 .. 5]
