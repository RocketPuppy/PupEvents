-- |This is a demonstration of how to write the Events for the Server
-- side of the code.
module Main  (
    main,
    -- * How to make Handlers
    -- $howtohandlers
    clickServer,
    unClickServer,
    clickHandlerServer,
    initHandlerServer,
    parsersServer,
    -- * Lookup Functions
    -- $lookups
    lookupPriorityServer,
    lookupHandlerServer,
    lookupParserServer,
    lookupUnHandlerServer,
    Event(..)
    ) where

import Text.Parsec
import qualified PupEventsServer as Server
import System.Environment

-- |Main entry point. You should pass in the address to connect to and
-- the number of priority levels desired.
main :: IO ()
main =
    do  args <- getArgs
        let ip = args !! 0
        let priorities = read (args !! 1) :: Int
        Server.server (Just ip) priorities lookupPriorityServer lookupUnHandlerServer lookupHandlerServer parsersServer

-- |Put your event models here
data Event = Click Float Float | Init
    deriving (Show)

-- $howtohandlers
-- Making new handlers is fairly simple.  You need several things to have a complete event representation.
-- 
-- * First is the parser to parse the event off the wire.  Currently the format of events on the wire is Name\0Arg1\0Arg2\0...\0ArgN\0\0.  You must follow this layout.  If you wish to change it you'll need to change the RecvEvents functions in the Client and Server modules.  Parsers have the type @Parsec String () Event@.
-- 
-- * Second is the unParser, which simply returns the string to be sent on the wire representing your event.  unParsers have the type @Event -> String@.
-- 
-- * Third is the handler. The handlers are separated by their suffix *Client or *Server.  The convenience functions also follow this pattern. The *Client functions should be in EventsClient, and the *Server functions should be in EventsServer. There's some duplication of data, but mostly the separation is to prevent inclusion of unwanted libraries (like OpenGL on the server). Event handlers should always return the event they are handling.  They have the type @Handler -> Event -> IO Event@.
-- 
-- After you have created your handler you need to add it to the convenience functions.  It's simple really, just follow the examples already there.

-----------
-- Click --
-----------
-- |Here we have the Click event parser. Most of the time your parsers
-- will be pretty simple. The protocol I like to follow is
-- @EventName\0Param1\0Param2\0...\0ParamN\0\0@. So the parameter
-- separator is \0 (the null character) and the event separator is
-- \0\0 (two null characters). 
clickServer :: Parsec String () Event
clickServer =
    do  string "Click"
        char '\0'
        p1 <- many $ oneOf "0123456789-+e."
        char '\0'
        p2 <- many $ oneOf "0123456789-+e."
        string "\0\0"
        return (Click (read p1) (read p2))

-- |The unClickClient method takes a Click event and turns it into a
-- string that when parsed yields the event used to create it.
unClickServer :: Event -> String
unClickServer (Click p1 p2) = "Click\0" ++ (show p1) ++ '\0':(show p2) ++ "\0\0"

-- |Here's the handler itself. All this handler does is negate the
-- points given by the Client and return them.
clickHandlerServer :: Event -> IO Event
clickHandlerServer (Click p1 p2) = return (Click (negate p1) (negate p2))

----------------
-- Initialize --
----------------
-- |This is the initHandler for the server, it doesn't actually do
-- anything.
initHandlerServer :: Event -> IO Event
initHandlerServer e@(Init) = return e

-- |This is a list of the event parsers to be used when trying to parse messages. If you don't add your event parsers to this list they won't get called!
parsersServer = [clickServer]

-- $lookups
-- These are convenience functions used to find values based on a
-- given Event. They work by pattern matching on an event and
-- returning the appropriate data.

-- |Returns the specified Event's priority level
lookupPriorityServer :: Event -> Int
lookupPriorityServer (Init) = 0
lookupPriorityServer (Click _ _ ) = 0

-- |Returns the specified Event's handler function. This has a weird type signature because it's returning a function.
lookupHandlerServer :: Event -> (Event -> IO Event)
lookupHandlerServer (Init) = initHandlerServer
lookupHandlerServer (Click _ _ ) = clickHandlerServer

-- |Returns the parser used to parse a specified Event string
lookupParserServer :: Event -> Parsec String () Event
lookupParserServer (Click _ _ ) = clickServer

-- |Returns the function used to "unhandle" an Event, that is convert
-- it to a string.
lookupUnHandlerServer :: Event -> (Event -> String)
lookupUnHandlerServer (Click _ _ ) = unClickServer
