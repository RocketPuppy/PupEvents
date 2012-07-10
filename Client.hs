module Client where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Text.Parsec
import Events
import PQueue

-- The client works much like the Dispatcher, except it doesn't listen
-- for connections from other places
-- Make a socket, connect to server, send and receive events
client ip =
    -- get address info
    do  addrinfos <- getAddrInfo Nothing (Just ip) (Just "1267")
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
        pqueue <- makeQueues 3
        -- fork threads for listening for events clientside
        mapM_ (\x -> forkIO $ x handle pqueue) eventListeners
        -- fork communication threads to server
        forkIO $ sendEvents handle pqueue
        forkIO $ recvEvents handle

-- These functions exist to listen for specific events on the Client and send them to the server
-- when new listeners are added add them to eventListeners or they won't get called
eventListeners = [acceptInput]
-- listen for keyboard input
acceptInput handle pqueue = forever $
    do  hSetBuffering stdin NoBuffering
        chars <- hGetContents stdin
        mapM_ (\x -> sendToServ x pqueue) chars
    where
        sendToServ char pqueue =
            do  putStrLn $ "char captured: " ++ show char
                let event = KeyPress char
                writeThing pqueue (lookupPriority event) event

sendEvents handle pqueue = forever $
    do  e <- getThing pqueue
        case e of
            Nothing -> return ()
            Just event -> do    putStrLn $ show event
                                hPutStr handle ((lookupUnHandler event) event)
                                hFlush handle

-- Receive events until the connection is closed, parse them, and handle them
recvEvents handle =
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
                                                    (lookupHandler a Client) a
        -- parsers is a global list of parsers imported from Events
        parseMsg = do choice parsers
        nullLines "" = []
        nullLines str = x:(nullLines xs)
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = (nullLines' (n+1) str)
