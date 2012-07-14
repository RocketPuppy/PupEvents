module PupEventsServer (server) where

import GHC.IO.Handle
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Concurrent
import Control.Monad
import Text.Parsec
import Network.Socket
import System.IO
import PupEventsPQueue

-- Make a socket, communication channels and start listening for connections
server Nothing priorities lookupPriority lookupUnHandler lookupHandler parsers = server (Just "0.0.0.0") priorities lookupPriority lookupUnHandler lookupHandler parsers
server ip priorities lookupPriority lookupUnHandler lookupHandler parsers = 
    do  -- get port
        addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                 ip (Just "1267")
        let serveraddr = head addrinfos
        -- create socket
        sock <- socket (addrFamily serveraddr) Stream 6
        -- bind to the address we're listening on
        bindSocket sock (addrAddress serveraddr)
        -- listen with maximum 5 queued requests
        listen sock 5
        chansSend <- newTVarIO []
        -- accept forever
        forever $ acceptCon sock chansSend priorities lookupPriority lookupUnHandler lookupHandler parsers

handleEvents handle pqueue lookupUnHandler lookupHandler = forever $
    do  event <- atomically $
            do  event <- getThing pqueue
                case event of
                    Nothing -> retry
                    Just event -> return event
        event' <- lookupHandler event event
        hPutStr handle (lookupUnHandler event' event')
        hFlush handle

-- accept a connection and fork a new thread to handle receiving events from it
-- after the connection is accepted, create a new channel for the dispatcher to
-- receive events from.
acceptCon sock chansSend priorities lookupPriority lookupUnHandler lookupHandler parsers =
    do  putStrLn "Accepting Connections"
        (connsock, clientaddr) <- accept sock
        putStrLn $ "Connection received from: " ++ show clientaddr
        connHandle <- socketToHandle connsock ReadWriteMode
        hSetBuffering connHandle NoBuffering
        hSetBinaryMode connHandle True
        pqueue <- makeQueues priorities
        forkIO (recvEvents connHandle pqueue lookupPriority parsers)
        forkIO (handleEvents connHandle pqueue lookupUnHandler lookupHandler)
        atomically  $   modifyTVar chansSend (\xs -> (clientaddr, pqueue):xs)

-- Receive events until the connection is closed, parse them, and push them on the
-- channel to the dispatcher
recvEvents handle pqueue lookupPriority parsers =
    -- I don't really understand how these two lines work, but I think its
    -- got something to do with lazy evalution.  they're from RWH.
    do  messages <- hGetContents handle
        mapM_ toDispatch (nullLines messages)
        hClose handle
    where
        toDispatch str = 
            case parse parseMsg "" str of
                Left e -> putStrLn $ "ParseError: " ++ show e ++ "\nString: " ++ show str
                Right a ->  atomically $ writeThing pqueue (lookupPriority a) a
        parseMsg =  choice parsers
        nullLines "" = []
        nullLines str = x:nullLines xs
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = nullLines' (n+1) str
