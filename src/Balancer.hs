module Balancer
  ( balance
  , newBalancer
  , Balancer
  )
where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Data.Heap                     as DH
import           Data.Maybe

import           Worker

type Pool a = DH.MinPrioHeap Int (Worker a)
-- | Load balancer to dispatch work that result in 'a'
data Balancer a = Balancer (Pool a) (TChan (Worker a))

data BalancerMsg a = RequestReceived (Request a)
                   | WorkerDone (Worker a)

balance
  :: TChan (Request a) -- ^ input channel to receive work from
  -> Balancer a        -- ^ Balancer
  -> IO ()
balance requestChan (Balancer workers doneChannel) = race_
  runWorkers
  (runBalancer workers)
 where
  runWorkers = mapConcurrently (`work` doneChannel) (map snd $ toList workers)
  runBalancer pool = do
    msg <-
      atomically
      $        (WorkerDone <$> readTChan doneChannel)
      `orElse` (RequestReceived <$> readTChan requestChan)

    newPool <- case msg of
      RequestReceived request -> dispatch pool request
      WorkerDone      worker  -> return $ completed pool worker

    putStrLn $ "Balancer: " ++ show newPool
    runBalancer newPool

dispatch
  :: Pool a      -- ^ worker pool
  -> Request a   -- ^ incoming request
  -> IO (Pool a) -- ^ new pool with updated worker priorities
dispatch pool request = do
  let ((p, w), pool') = fromJust $ DH.view pool
  schedule w request
  return $ DH.insert (p + 1, w) pool'

completed
  :: Pool a   -- ^ worker pool
  -> Worker a -- ^ worker that finished the task
  -> Pool a   -- ^ new pool with updated worker priorities
completed pool worker =
  let (p', pool') = DH.partition (\item -> snd item == worker) pool
      [(p, w)]    = toList p'
  in  DH.insert (p - 1, w) pool'

newBalancer
  :: TChan (Request a) -- ^ input channel to receive data from
  -> Int               -- ^ number of workers
  -> IO (Balancer a)
newBalancer channel n = do
  workers     <- forM [1 .. n] newWorker
  doneChannel <- newTChanIO
  return $ Balancer (fromList $ zip (repeat 0) workers) doneChannel
