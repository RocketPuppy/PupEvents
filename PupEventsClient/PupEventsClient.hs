module PupEventsClient (client) where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Text.Parsec
import PupEventsPQueue

-- The client works much like the Dispatcher, except it doesn't listen
-- for connections from other places
-- Make a socket, connect to server, send and receive events
client Nothing priorities lookupPriority lookupUnHandler parsers = client (Just "localhost") priorities lookupPriority lookupUnHandler parsers
client ip priorities lookupPriority lookupUnHandler parsers=
    -- get address info
    do  addrinfos <- getAddrInfo Nothing ip (Just "1267")
        let serveraddr = head addrinfos
        -- create socket
        sock <- socket (addrFamily serveraddr) Stream 6
        setSocketOption sock KeepAlive 1
        -- connect to the server
        connect sock (addrAddress serveraddr)
        -- convert to handle for convenience
        handle <- socketToHandle sock ReadWriteMode
        hSetBuffering handle NoBuffering
        -- create pqueue
        outqueue <- makeQueues priorities
        inqueue <- makeQueues priorities
        -- fork communication threads to server
        forkOS $ sendEvents handle outqueue lookupUnHandler
        forkOS $ recvEvents handle inqueue lookupPriority parsers
        -- outqueue is the outgoing messages to the server
        -- inqueue is the incoming messages from the server
        return (outqueue, inqueue)

-- send events to the server for processing
sendEvents handle pqueue lookupUnHandler = forever $
    do  event <- atomically $
            do  e <- getThing pqueue
                case e of
                    Nothing -> retry
                    Just event -> return event
        hPutStr handle ((lookupUnHandler event) event)
        hFlush handle

-- Receive events until the connection is closed, parse them, and put them on the out queue
recvEvents handle pqueue lookupPriority parsers =
    -- I don't really understand how these two lines work, but I think its
    -- got something to do with lazy evalution.  they're from RWH.
    do  messages <- hGetContents handle
        mapM_ toDispatch (nullLines messages)
        hClose handle
    where
        toDispatch str = 
            do  case (parse parseMsg "" str) of
                    Left e -> putStrLn $ "ParseError: " ++ show e ++ "\nString: " ++ show str
                    Right event ->  atomically $ writeThing pqueue (lookupPriority event) event

        -- parsers is a global list of parsers imported from Events
        parseMsg = do choice parsers
        nullLines "" = []
        nullLines str = x:(nullLines xs)
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = (nullLines' (n+1) str)
