module Dispatcher where

import GHC.IO.Handle
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Concurrent
import Control.Monad
import Text.Parsec
import Network.Socket
import System.IO
import Listener
import Handler

-- Make a socket, communication channels and start listening for connections
dispatcher Nothing = dispatcher (Just "0.0.0.0")
dispatcher (Just ip) = 
    do  -- get port
        addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                 (Just ip) (Just "1267")
        let serveraddr = head addrinfos
        -- create socket
        sock <- socket (addrFamily serveraddr) Stream 6
        -- bind to the address we're listening on
        bindSocket sock (addrAddress serveraddr)
        -- listen with maximum 5 queued requests
        listen sock 5
        chansSend <- newTVarIO []
        -- accept forever
        forever $ acceptCon sock chansSend

handleEvents chan = forever $
    do  event <- atomically $ readTChan chan
        putStrLn $ "Event received from Dispatcher"
        (lookupHandler event) $ event

-- accept a connection and fork a new thread to handle receiving events from it
-- after the connection is accepted, create a new channel for the dispatcher to
-- receive events from.
acceptCon sock chansSend =
    do  (connsock, clientaddr) <- accept sock
        putStrLn $ "Connection received from: " ++ show clientaddr
        connHandle <- socketToHandle connsock ReadMode
        hSetBuffering connHandle NoBuffering
        hSetBinaryMode connHandle True
        chanSend <- newTChanIO
        forkIO (recvEvents connHandle chanSend)
        forkIO (handleEvents chanSend)
        atomically  $   do  modifyTVar chansSend (\xs -> (clientaddr, chanSend):xs)

-- Receive events until the connection is closed, parse them, and push them on the
-- channel to the dispatcher
recvEvents handle chan =
    -- I don't really understand how these two lines work, but I think its
    -- got something to do with lazy evalution.  they're from RWH.
    do  messages <- hGetContents handle
        mapM_ toDispatch (nullLines messages)
        hClose handle
    where
        toDispatch str = 
            do  putStrLn $ "Message received: " ++ str
                do case (parse parseMsg "" str) of
                                    Left e -> putStrLn $ "ParseError: " ++ show e ++ "\nString: " ++ show str
                                    Right a ->  do  putStrLn "Parsed Message"
                                                    atomically $ writeTChan chan a
        parseMsg = do choice (map (\(EventParser p) -> p) listenerParsers)
        nullLines "" = []
        nullLines str = x:(nullLines xs)
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = (nullLines' (n+1) str)
