module PQueue where

import Control.Concurrent.STM
import Control.Monad

newtype Priority = Priority Int
-- create pqueue
makeQueues num =
    do  replicateM 3 newTChanIO

-- get the next thing in the queuern thing
getThing [] = return Nothing
getThing queues = 
    do  event <- tryReadTChan $ head queues
        case event of
            Nothing -> getThing $ tail queues
            Just e -> return (Just e)

-- write something to the queue
writeThing queues priority thing =
    do  if length queues < priority && priority > 0
            then putStrLn "Not writing"
            else do putStrLn "Writing"
                    atomically $ do writeTChan (queues !! priority) thing
