module Events where

import GHC.IO.Handle
import System.IO
import Control.Monad
import Control.Concurrent.STM.TChan
import Text.Parsec
import PQueue
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT as GLUT

data EventPackage = EventPackage EventPrinter EventParser

-- for getting from the wire
data EventParser = EventParser (Parsec String () Event)

-- for printing on the wire
data EventPrinter = EventPrinter (Event -> String)

-- Internal Event Representation
data Event = KeyPress Char | Click Float Float | Init
    deriving (Show)

data HandlerMode = Client | Server

-- Events and their structures, both internal and external representation

-- | Making new handlers is fairly simple.  You need several things to have a complete event representation.
-- * First is the parser to parse the event off the wire.  Currently the format of events on the wire is Name\0Arg1\0Arg2\0...\0ArgN\0\0.  You must follow this layout.  If you wish to change it you'll need to change the RecvEvents functions in the Client and Server modules.  Parsers have the type @Parsec String () Event@.
-- * Second is the unParser, which simply returns the string to be sent on the wire representing your event.  unParsers have the type @Event -> String@.
-- * Third is the handler.  There is the ability to do separate things depending on if you're handling the event on the Client or the Server.  Event handlers should always return the event they are handling.  They have the type @Handler -> Event -> IO Event@.
-- After you have created your handler you need to add it to the convenience functions.  It's simple really, just follow the examples already there.

-----------
-- Click --
-----------
click :: Parsec String () Event
click =
    do  string "Click"
        char '\0'
        p1 <- 
            do  a <- option "" $ string "-"
                b <- many digit
                c <- option "" $ string "."
                d <- option "" $ many digit
                return (a ++ b ++ c ++ d)
        char '\0'
        p2 <- 
            do  a <- option "" $ string "-"
                b <- many digit
                c <- option "" $ string "."
                d <- option "" $ many digit
                return (a ++ b ++ c ++ d)
        string "\0\0"
        return (Click (read p1) (read p2))

unClick :: Event -> String
unClick (Click p1 p2) = "Click\0" ++ (show p1) ++ '\0':(show p2) ++ "\0\0"

clickHandler :: HandlerMode -> Event -> IO Event
clickHandler Server (Click p1 p2) = return (Click (negate p1) (negate p2))
clickHandler Client event@(Click p1 p2) =
    do  GL.preservingMatrix $ GL.renderPrimitive GL.Points $ do
            GL.color (GL.Color4 1 0 1 1 :: GL.Color4 Float)
            GL.vertex (GL.Vertex2 p1 p2 :: GL.Vertex2 Float)
        putStrLn $ "handling click"
        return event
----------------
-- Initialize --
----------------
initHandler Server _ = undefined
initHandler Client (Init) =
    do  GL.clearColor $= GL.Color4 0 0 0 1
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.lineSmooth $= GL.Enabled
        GL.pointSmooth $= GL.Enabled
        GL.polygonSmooth $= GL.Enabled
        GL.clear [GL.ColorBuffer]
        GL.loadIdentity
        return Init

-- If you don't add your event parsers to this list they won't get called!
parsers = [click]

-- Convenience functions. Pattern match on the Event constructor and return the wanted data
lookupPriority (Init) = 0
lookupPriority (Click _ _ ) = 0

lookupHandler (Init) mode = initHandler mode
lookupHandler (Click _ _ ) mode = clickHandler mode

lookupParser (Click _ _ ) = click

lookupUnHandler (Click _ _ ) = unClick
