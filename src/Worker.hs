module Worker
  ( Request(..)
  , Worker
  , newWorker
  , schedule
  , work
  )
where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad                  ( forever )
import           Data.Maybe

data Request a = Request (IO a) (TChan a)
data Worker a = Worker Int (TChan (Request a)) deriving Eq

instance Show (Worker a) where
    show (Worker i _) = "Worker " ++ show i

work :: Worker a -> TChan (Worker a) -> IO ()
work (Worker workerId requestChan) doneChannel = forever $ do
  Request task resultChan <- atomically $ readTChan requestChan
  result                  <- task
  atomically $ do
    writeTChan resultChan  result
    writeTChan doneChannel (Worker workerId requestChan)

newWorker i = do
  wc <- newTChanIO
  return $ Worker i wc

schedule :: Worker a -> Request a -> IO ()
schedule (Worker i c) request = atomically $ writeTChan c request
