module Lib
  ( start
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( race_
                                                , async
                                                )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad                  ( forever )
import           System.Random                  ( getStdRandom
                                                , randomR
                                                )

import           Balancer
import           Worker

randomTask :: IO Int
randomTask = do
  delayMs <- getStdRandom $ randomR (1, 3000)
  threadDelay (delayMs * 1000)
  return delayMs

requester :: TChan (Request Int) -> IO ()
requester balancer = forever $ do
  delayMs <- getStdRandom $ randomR (1, 1000)
  threadDelay (delayMs * 1000) -- simulating random load

  resultChan <- newTChanIO
  atomically $ writeTChan balancer (Request randomTask resultChan)

  -- wait for the result
  async $ atomically $ readTChan resultChan

start :: IO ()
start = do
  chan     <- newTChanIO
  balancer <- newBalancer chan 3

  race_ (balance chan balancer) (requester chan)
