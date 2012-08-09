{-# OPTIONS_HADDOCK ignore-exports #-}
-- |The Server module of the PupEvents framework is designed to be run from
-- the file that specifies the Events that you are handling. The only thing
-- that would need to be written to use this is the Events file, it does
-- not depend on the application code.
module PupEventsServer (server) where

import GHC.IO.Handle
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Concurrent
import Control.Monad
import Control.Exception.Base (finally)
import qualified Control.Exception as C
import Text.Parsec
import Network.Socket
import System.IO
import PupEventsPQueue
import Data.Functor.Identity

-- Make a socket, communication channels and start listening for connections
-- |The main entry point of the program. This ends by forever calling 'acceptCon'.
server ::   Maybe [Char] -- ^ The address to listen on, if it's not given default to "0.0.0.0".
            -> Int -- ^ The number of priority levels to create the PQueue with
            -> (t -> Int) -- ^ The function to look up the priority level of an event
            -> (t1 -> t1 -> String) -- ^ The lookup function to convert an event to a string representation
            -> (t -> t -> IO t1) -- ^ The lookup function to look up the handler for an event
            -> [ParsecT [Char] () Data.Functor.Identity.Identity t] -- ^ The list of parsers to try and parse events with
            -> Maybe t -- ^Optional. The event to put on the pqueue when a client disconnects
            -> IO b
server Nothing priorities lookupPriority lookupUnHandler lookupHandler parsers dcEvent = server (Just "0.0.0.0") priorities lookupPriority lookupUnHandler lookupHandler parsers dcEvent 
server ip priorities lookupPriority lookupUnHandler lookupHandler parsers dcEvent = 
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
        -- accept forever
        finally (forever $ acceptCon sock priorities lookupPriority lookupUnHandler lookupHandler parsers dcEvent) (shutdown sock ShutdownBoth)

-- |Checks the given PQueue for an event to handle and calls the handler
-- for it, then sends the event returned by the handler to the client.
handleEvents :: Handle -- ^ The handle to send the handled event on
                -> PQueue t -- ^ The PQueue on which to listen for events to be handled
                -> (t1 -> t1 -> String) -- ^ The lookup function to find the function that returns a string representation of the event.
                -> (t -> t -> IO t1) -- ^ The lookup function that returns the event handler
                -> IO ()
handleEvents handle pqueue lookupUnHandler lookupHandler =
    do  event <- atomically $
            do  event <- getThing pqueue
                case event of
                    Nothing -> retry
                    Just event -> return event
        event' <- lookupHandler event event
        C.catch (
            do  hPutStr handle (lookupUnHandler event' event')
                hFlush handle
                handleEvents handle pqueue lookupUnHandler lookupHandler
            ) (\e -> do let err = show (e :: C.IOException)
                        hPutStr stderr ("Client disconnected")
                        )


-- accept a connection and fork a new thread to handle receiving events from it
-- after the connection is accepted, create a new channel for the dispatcher to
-- receive events from.
-- |Listens for an incoming connection. When it gets one it spawns two threads for the connection, one using 'handleEvents' and the other using 'recvEvents'.
acceptCon ::    Socket -- ^ The socket to listen for incoming connections on
                -> Int -- ^ The number of priorities for the PQueues to have
                -> (t -> Int) -- ^ The function to lookup the priority level of an event
                -> (t1 -> t1 -> String) -- ^ The function to lookup the string representation for an event
                -> (t -> t -> IO t1) -- ^ The function to lookup the handler for the event
                -> [ParsecT [Char] () Data.Functor.Identity.Identity t] -- ^ A list of parsers to use when trying to parse Events
                -> Maybe t -- ^Optional. The event to put on the pqueue when a client disconnects
                -> IO ThreadId
acceptCon sock priorities lookupPriority lookupUnHandler lookupHandler parsers dcEvent =
    do  hPutStr stderr "Accepting Connections\n"
        (connsock, clientaddr) <- accept sock
        hPutStr stderr $ "Connection received from: " ++ show clientaddr ++ "\n"
        connHandle <- socketToHandle connsock ReadWriteMode
        hSetBuffering connHandle NoBuffering
        hSetBinaryMode connHandle True
        pqueue <- makeQueues priorities
        forkIO (recvEvents connHandle pqueue lookupPriority parsers dcEvent)
        forkIO (handleEvents connHandle pqueue lookupUnHandler lookupHandler)

-- Receive events until the connection is closed, parse them, and push them on the
-- channel to the dispatcher
-- |Listens for events from the Client and sends them to the 'handleEvents' thread.
recvEvents ::   Handle -- ^ The handle to listen for events on
                -> PQueue a -- ^ The PQueue used to send events to the 'handleEvents' thread
                -> (a -> Int) -- ^ The function to lookup the priority level of an event
                -> [ParsecT [Char] () Data.Functor.Identity.Identity a] -- ^ A list of parsers to use when trying to parse Events
                -> Maybe a -- ^Optional. The event to put on the pqueue when a client disconnects
                -> IO ()
recvEvents handle pqueue lookupPriority parsers dcEvent =
    -- I don't really understand how these two lines work, but I think its
    -- got something to do with lazy evalution.  they're from RWH.
    do  messages <- hGetContents handle
        mapM_ toDispatch (nullLines messages)
        hClose handle
        case dcEvent of
            Just x -> atomically $ writeThing pqueue (lookupPriority x) x
            Nothing -> return ()
    where
        toDispatch str = 
            case parse parseMsg "" str of
                Left e -> hPutStr stderr $ "ParseError: " ++ show e ++ "\nString: " ++ show str ++ "\n"
                Right a ->  atomically $ writeThing pqueue (lookupPriority a) a
        parseMsg =  choice parsers
        nullLines "" = []
        nullLines str = x:nullLines xs
            where   (x, xs) = splitAt (nullLines' 0 str) str
                    nullLines' n [] = n
                    nullLines' n ('\0':'\0':str) = n+2
                    nullLines' n (s:str) = nullLines' (n+1) str
