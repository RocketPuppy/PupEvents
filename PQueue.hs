
-- create pqueue
do  p1 <- newTChanIO
    p2 <- newTChanIO
    p3 <- newTChanIO
    pQueue <- newTVarIO (p1, p2, p3)

--read pqueue
atomically $
    do  (p1, p2, p3) <- readTVar pQueue
        
