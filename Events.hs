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

-- If you don't add your event parsers to this list they won't get called!
parsers = [keyPress, click]

--------------
-- KeyPress --
--------------
-- the un* functions should generate the representation of the event that goes on the wire.
unKeyPress (KeyPress c) = "KeyPress\0" ++ c:"\0\0"

-- We parse the event off the wire with parsec
keyPress :: Parsec String () Event
keyPress = do string "KeyPress"
              char '\0'
              c <- option '\0' (noneOf "\0")
              return (KeyPress c)

-- Each event should have a server and a client handler
-- Server handlers should return either the original event, or the new event from the handler
-- Client handlers should not return anything (return values are ignored)
keyPressHandler Server (KeyPress c) = 
    do  putStrLn $ "keypress from client: " ++ show c
        return (KeyPress c)
keyPressHandler Client (KeyPress c) =
    do  putStrLn $ "keypress from server: " ++ show c
        return (KeyPress c)


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

unClick (Click p1 p2) = "Click\0" ++ (show p1) ++ '\0':(show p2) ++ "\0\0"

clickHandler :: HandlerMode -> Event -> IO Event
clickHandler Server (Click p1 p2) = return (Click (negate p1) (negate p2))
clickHandler Client event@(Click p1 p2) =
    do  GL.preservingMatrix $ GL.renderPrimitive GL.Points $ do
            GL.color (GL.Color4 0 1 0 1 :: GL.Color4 Float)
            GL.vertex (GL.Vertex2 p1 p2 :: GL.Vertex2 Float)
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

-- Convenience functions. Pattern match on the Event constructor and return the wanted data
lookupPriority (Init) = 0
lookupPriority (KeyPress _) = 0
lookupPriority (Click _ _ ) = 0
lookupHandler (Init) mode = initHandler mode
lookupHandler (Click _ _ ) mode = clickHandler mode
lookupHandler (KeyPress _) mode = keyPressHandler mode
lookupParser (Click _ _ ) = click
lookupParser (KeyPress _) = keyPress
lookupUnHandler (Click _ _ ) = unClick
lookupUnHandler (KeyPress _) = unKeyPress

-- These functions exist to listen for specific events on the Client and send them to the server
-- when new listeners are added add them to eventListeners or they won't get called
eventListeners = [acceptInput]
-- listen for keyboard input
acceptInput pqueue = forever $
    do  hSetBuffering stdin NoBuffering
        chars <- hGetContents stdin
        mapM_ (\x -> sendToServ x pqueue) chars
    where
        sendToServ char pqueue =
            do  putStrLn $ "char captured: " ++ show char
                let event = KeyPress char
                writeThing pqueue (lookupPriority event) event