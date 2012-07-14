module PupEventsPQueue (makeQueues, getThing, writeThing) where

import Control.Concurrent.STM
import Control.Monad

newtype Priority = Priority Int
-- create pqueue
makeQueues num = replicateM num newTChanIO

-- get the next thing in the queuern thing
getThing [] = return Nothing
getThing queues = 
    do  event <- tryReadTChan $ head queues
        case event of
            Nothing -> getThing $ tail queues
            Just e -> return (Just e)

-- write something to the queue
writeThing queues priority thing =
    unless (length queues < priority && priority > 0) $
        writeTChan (queues !! priority) thing
