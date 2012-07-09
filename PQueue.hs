module PQueue where

-- create pqueue
makeQueues num =
    do  return $ take num $ mapM (\_ -> newTChan) $ repeat 1
    where
        makeQueues' num [] = do 
    do  p1 <- newTChanIO
        p2 <- newTChanIO
        p3 <- newTChanIO
        return (p1, p2, p3)

-- get first thing in pQueue
getElem (Nothing, Nothing, p3) =
    do  event <- atomically $ tryReadTChan p3
        case event of 
            Nothing -> return Nothing
            Just e -> return e
getElem (Nothing, p2, p3) =
    do  event <- atomically $ tryReadTChan p2
        case event of
            Nothing -> getElem (Nothing, Nothing, p3)
            Just e -> return e
getElem (p1, p2, p3) =
    do  event <- atomically $ tryReadTChan p1
        case event of
            Nothing -> getElem (Nothing, p2, p3)
            Just e -> return e
