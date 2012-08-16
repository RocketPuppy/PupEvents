{-# OPTIONS_HADDOCK ignore-exports #-}
-- |The Clients module in the PupEvents framework is used by the main
-- application code to send events to the server. Its main function,
-- 'client' returns a pair of "PQueues" that the application uses to send
-- and receive events (written following the specification defined in the
-- Events module).
module PupEventsClient (client) where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as C
import Control.Exception.Base (finally)
import Control.Monad
import Text.Parsec
import PupEventsPQueue
import Data.Functor.Identity

-- |The client function is the main entry point for the client code. It creates a socket, spawns two processes ('sendEvents' and 'recvEvents') to handle outgoing and incoming events, and returns the queues used to communicate with those processes.
client ::   Maybe [Char] -- ^ The address to connect to. If ommitted we connect to 'localhost'
            -> Int -- ^ The number of priorities in the PQueue
            -> String -- ^ The port to start the client on.
            -> (a -> Int) -- ^ A function to return the priority level of an event
            -> (t -> t -> String) -- ^ A function to return the string representation of an event
            -> [ParsecT [Char] () Data.Functor.Identity.Identity a] -- ^ A list of parsers that return Event objects
            -> IO (PQueue t, PQueue a, IO ()) -- ^ We return a pair of "PQueues" to use in communicating events. The first is for all events going to the server, the second is for events coming from the server. We also return a function to manually close both the socket and handle we use to connect to the server.
client Nothing priorities port lookupPriority lookupUnHandler parsers = client (Just "localhost") priorities port lookupPriority lookupUnHandler parsers
client ip priorities port lookupPriority lookupUnHandler parsers=
    -- get address info
    do  addrinfos <- getAddrInfo Nothing ip (Just port)
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
        t1 <- forkOS $ sendEvents handle outqueue lookupUnHandler
        t2 <- forkOS $ recvEvents handle inqueue lookupPriority parsers
        -- outqueue is the outgoing messages to the server
        -- inqueue is the incoming messages from the server
        return (outqueue, inqueue, {-(hPutStr stderr "shutting down client...\n" >>-} killThread t1 >> killThread t2 >> hClose handle {->> hPutStr stderr "client shutdown.\n")-})

-- |The sendEvents function handles the sending of all outgoing events to the server. It checks for an event, blocks if none is available, and sends it.
sendEvents ::   Handle -- ^ Handle to send events on
                -> PQueue t -- ^ "PQueue" to listen on
                -> (t -> t -> String) -- ^ A function to convert an Event to a string
                -> IO ()
sendEvents handle pqueue lookupUnHandler = finally
    (forever $
        do  event <- atomically $
                do  e <- getThing pqueue
                    case e of
                        Nothing -> retry
                        Just event -> return event
            hPutStr handle (lookupUnHandler event event)
            hFlush handle
            sendEvents handle pqueue lookupUnHandler)
    (do hClose handle
        --hPutStr stderr ("sendEvents exiting...\n")
        )

-- |The recvEvents function receives events until the connection is closed, parses them, and puts them on the queue to be handled by the application.
recvEvents ::   Handle -- ^ Handle listen on
                -> PQueue a -- ^ "PQueue" to send events on
                -> (a -> Int) -- ^ Function to lookup the priority level of an event
                -> [ParsecT [Char] () Data.Functor.Identity.Identity a] -- ^ A list of parsers to apply to parse an event
                -> IO ()
recvEvents handle pqueue lookupPriority parsers = finally
    -- I don't really understand how these two lines work, but I think its
    -- got something to do with lazy evalution.  they're from RWH.
    (do messages <- hGetContents handle
        mapM_ toDispatch (nullLines messages))
    (do hClose handle
        {-hPutStr stderr "recvEvents exiting...\n"-})
    where
        -- |Attempts to parse an event and send it to the queue.
        toDispatch str = 
            case parse parseMsg "" str of
                Left e -> hPutStr stderr $ "ParseError: " ++ show e ++ "\nString: " ++ show str ++ "\n"
                Right event ->  atomically $ writeThing pqueue (lookupPriority event) event

        -- |Applies the parsers given in 'client' until one of them succeeds.
        parseMsg =  choice parsers
        -- |Separates a string on the character sequence \"\0\0\"
        nullLines "" = []
        nullLines str = x:nullLines xs
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = nullLines' (n+1) str
