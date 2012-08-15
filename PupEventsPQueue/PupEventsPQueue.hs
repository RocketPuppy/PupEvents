-- |Implements Priority Queues intended to be used with the STM module.
module PupEventsPQueue (makeQueues, getThing, writeThing, PQueue) where

import Control.Concurrent.STM
import Control.Monad

-- |A simple alias so we have pretty types.
type PQueue a = [TChan a]

-- |Returns an empty PQueue with the specified number of priority levels.
makeQueues ::   Int -- ^ The number of priority levels
                -> IO (PQueue a) -- ^ The PQueue
makeQueues num = replicateM num newTChanIO

-- |Recursively tries to read each priority level until it finds something.
-- If it can't find any events it returns Nothing. Call using @atomically $
-- getThing pqueue@.
getThing :: PQueue a -- ^ The PQueue to search
            -> STM (Maybe a) -- ^ Just the item or Nothing.
getThing [] = return Nothing
getThing queues = 
    do  event <- tryReadTChan $ head queues
        case event of
            Nothing -> getThing $ tail queues
            Just e -> return (Just e)

-- |Writes the given something to the given PQueue at the specified
-- priority level. It does some checking to make sure we have a valid
-- priority level. Call using @atomically $ writeThing pqueue priority thing@.
writeThing ::   PQueue a -- ^ The PQueue to write to
                -> Int -- ^ The priority level
                -> a -- ^ The thing to write
                -> STM ()
writeThing queues priority thing =
    unless (length queues < priority && priority > 0) $
        writeTChan (queues !! priority) thing
