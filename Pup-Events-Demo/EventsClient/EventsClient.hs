-- |This is a demonstration of how to write the Events for the Client
-- side of the code. These events are used in an OpenGL application so
-- we have some OpenGL code here in the handlers.
module EventsClient (
    doClient,
    -- * How to make Handlers
    -- $howtohandlers
    clickClient,
    unClickClient,
    clickHandlerClient,
    initHandlerClient,
    parsersClient,
    -- * Lookup Functions
    -- $lookups
    lookupPriorityClient,
    lookupHandlerClient,
    lookupParserClient,
    lookupUnHandlerClient,
    Event(..)
    )
where

import GHC.IO.Handle
import System.IO
import Control.Monad
import Control.Concurrent.STM.TChan
import Text.Parsec
import PupEventsPQueue
import qualified PupEventsClient as Client
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified PupEventsClient

-- |The doClient method is how we start the client side of the
-- Pup-Events framework. It's called by the main application.
doClient ip priorities =
    do  (q1, q2, _) <- Client.client ip priorities "1267" lookupPriorityClient lookupUnHandlerClient parsersClient
        return (q1, q2)

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
clickClient :: Parsec String () Event
clickClient =
    do  string "Click"
        char '\0'
        p1 <- many $ oneOf "0123456789+-e."
        char '\0'
        p2 <- many $ oneOf "0123456789+-e."
        string "\0\0"
        return (Click (read p1) (read p2))

-- |The unClickClient method takes a Click event and turns it into a
-- string that when parsed yields the event used to create it.
unClickClient :: Event -> String
unClickClient (Click p1 p2) = "Click\0" ++ (show p1) ++ '\0':(show p2) ++ "\0\0"

-- |Here's the handler itself. All this handler does is paint the
-- point specified by the Click event. It's some pretty simple OpenGL
-- code.
clickHandlerClient :: Event -> IO Event
clickHandlerClient event@(Click p1 p2) =
    do  GL.preservingMatrix $ GL.renderPrimitive GL.Points $ do
            GL.color (GL.Color4 1 0 1 1 :: GL.Color4 Float)
            GL.vertex (GL.Vertex2 p1 p2 :: GL.Vertex2 Float)
        return event
----------------
-- Initialize --
----------------
-- |This is an event I wrote that would initialize the display.
-- Unfortunately I was having problems when I had it as an event so it
-- never gets used anymore. However I left it in here to illustrate
-- having multiple events.
initHandlerClient :: Event -> IO Event
initHandlerClient (Init) =
    do  GL.clearColor $= GL.Color4 0 0 0 1
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.lineSmooth $= GL.Enabled
        GL.pointSmooth $= GL.Enabled
        GL.polygonSmooth $= GL.Enabled
        GL.clear [GL.ColorBuffer]
        GL.loadIdentity
        return Init

-- |This is a list of the event parsers to be used when trying to parse messages. If you don't add your event parsers to this list they won't get called!
parsersClient = [clickClient]

-- $lookups
-- These are convenience functions used to find values based on a
-- given Event. They work by pattern matching on an event and
-- returning the appropriate data.

-- |Returns the specified Event's priority level
lookupPriorityClient :: Event -> Int
lookupPriorityClient (Init) = 0
lookupPriorityClient (Click _ _ ) = 0

-- |Returns the specified Event's handler function. This has a weird type signature because it's returning a function.
lookupHandlerClient :: Event -> (Event -> IO Event)
lookupHandlerClient (Init) = initHandlerClient
lookupHandlerClient (Click _ _ ) = clickHandlerClient

-- |Returns the parser used to parse a specified Event string
lookupParserClient :: Event -> Parsec String () Event
lookupParserClient (Click _ _ ) = clickClient

-- |Returns the function used to "unhandle" an Event, that is convert
-- it to a string.
lookupUnHandlerClient :: Event -> (Event -> String)
lookupUnHandlerClient (Click _ _ ) = unClickClient
