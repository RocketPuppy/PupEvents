-- |This is a program which uses OpenGL and the PupEvents framework to
-- demonstrate an example usage pattern. It simply takes a click in
-- the window, sends it to the server (which negates the coordinates),
-- and then paints the original and new Click coordinates with a point.
module Main where

import Graphics.Rendering.OpenGL.GL (($=), ($=!), makeGettableStateVar)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT as GLUT
import qualified PupEventsPQueue as PQ
import qualified EventsClient as Events
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.Environment

-- |Initial OpenGL setup function. The interesting thing here is that
-- we fork a thread using 'checkEvents' to check for events.
main :: IO ()
main = 
    do  args <- getArgs
        let ip = args !! 0
        let priorities = read (args !! 1) :: Int
        (progname, _) <- GLUT.getArgsAndInitialize
        (outqueue, inqueue) <- Events.doClient (Just ip) priorities
        GLUT.createWindow "Hello World"
        GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBAMode]
        GLUT.keyboardMouseCallback $=! Just (keyboardMouse inqueue outqueue)
        GLUT.displayCallback $=! render
        forkOS $ checkEvents inqueue
        GL.clearColor $= GL.Color4 0 0 0 1
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.lineSmooth $= GL.Enabled
        GL.pointSmooth $= GL.Enabled
        GL.polygonSmooth $= GL.Enabled
        GL.clear [GL.ColorBuffer]
        GL.normalize $= GL.Enabled
        GLUT.mainLoop

-- |This is very simple, since all of the drawing is done by Event
-- handlers. We set this function as the display callback and call it
-- after each event has been handled.
render :: IO ()
render = GLUT.swapBuffers

-- |This checks the inqueue returned by the Client module for Events,
-- blocking if it can't find one. We fork this off because if the
-- functionality here resided in the display callback it would block
-- the entire application from doing anything while it was waiting for
-- an event, including calling the display callback. Using this method
-- we handle Events as the come in and call the display callback to
-- swap the buffers when it's done.
checkEvents ::  PQ.PQueue Events.Event -- ^ The queue to check on
                -> IO b
checkEvents pqueue = forever $
    do  event <- atomically $
            do  e <- PQ.getThing pqueue
                case e of
                    Nothing -> retry
                    Just event -> return event
        putStrLn $ "Got event"
        (Events.lookupHandlerClient event) event
        GLUT.postRedisplay Nothing

-- |This is the keyboardMouse callback that gets used by OpenGL. We
-- first normalize the coordinates to a -1,1 range, then we push a
-- Click event to the incoming queue (not all events on the queue have
-- to come from the Server!) and also on the outgoing queue so the
-- server can handle it.
keyboardMouse ::    PQ.PQueue Events.Event -- ^ The incoming events queue
                    -> PQ.PQueue Events.Event -- ^ The outgoing events queue
                    -> GLUT.Key
                    -> GLUT.KeyState
                    -> t
                    -> GLUT.Position
                    -> IO ()
keyboardMouse iqueue oqueue (GLUT.MouseButton GLUT.LeftButton) GLUT.Down _ (GLUT.Position x y) =
    do  (p1, p2) <- normalizeXY (fromIntegral x) (fromIntegral y)
        putStrLn $ "click coords: " ++ show x ++ ", " ++ show y
        putStrLn $ "normalized: " ++ show p1 ++ ", " ++ show p2
        let event = Events.Click p1 p2
        putStrLn $ "KeyboardMouse callback"
        atomically $ PQ.writeThing oqueue (Events.lookupPriorityClient event) event
        atomically $ PQ.writeThing iqueue (Events.lookupPriorityClient event) event
        putStrLn $ "wrote things"
keyboardMouse _ _ _ _ _ _ = return ()

-- |This is a function to normalize coordinates that are given with
-- respect to window dimensions. OpenGL likes it's primitives to have
-- drawing coordinates ranging from -1 to 1 on the x or y axis.
normalizeXY ::  (Fractional t, Fractional t1) => 
                t -- ^ X coordinate
                -> t1 -- ^ Y coordinate
                -> IO (t, t1) -- ^ Normalized coordinates (x, y)
normalizeXY x y = 
    do  (_, (GLUT.Size width height)) <- GLUT.get GLUT.viewport
        let w = fromIntegral width
        let h = fromIntegral height
        return ((x-w/2.0)/(w/2.0), (y-h/2.0)/((-h)/2.0))
