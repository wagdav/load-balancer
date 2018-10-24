module Lib
    ( someFunc
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent        (threadDelay)
import System.Random             (getStdRandom, randomR)
import Text.Printf               (printf)
import Control.Monad               (forever)

someFunc :: IO ()
someFunc = start


type Result = Int

data Request = Request (IO ()) (TChan Result)
data Worker = Worker (TChan Request) Int Int
data Balancer = Balancer [Worker]


work :: IO ()
work = do
    delayMs <- getStdRandom $ randomR (1, 1000)
    printf "Worker starts...\n"
    threadDelay (delayMs * 1000)
    printf "Worker done!\n"

requester :: TChan Request -> IO ()
requester balancer = forever $ do
    delayMs <- getStdRandom $ randomR (1, 2000)
    threadDelay (delayMs * 1000) -- simulating random load
    printf "Requester sending work...\n"

    resultChan <- newTChanIO
    atomically $ writeTChan balancer (Request work resultChan)
    result <- atomically $ readTChan resultChan

    printf "Requester received result: %d\n" result

worker :: TChan Request -> IO ()
worker requestChan = forever $ do
    Request task resultChan <- atomically $ readTChan requestChan
    task
    atomically $ writeTChan resultChan 42

balance :: Balancer -> TChan Request -> IO ()
balance balancer requestChan = do
    request <- atomically $ readTChan requestChan
    newBalancer <- dispatch balancer request
    balance newBalancer requestChan

dispatch :: Balancer -> Request -> IO Balancer
dispatch (Balancer []) _ = return (Balancer [])
dispatch (Balancer (Worker requestChan x y:ws)) request = do
    atomically $ writeTChan requestChan request
    return $ Balancer (ws ++ [Worker requestChan x y])


start :: IO ()
start = do
    c <- newTChanIO

    async $ requester c
    async $ worker c

    let workers = [Worker c 0 0]
    balance (Balancer workers) c
    return ()
