import Graphics.Rendering.OpenGL.GL (($=), ($=!), makeGettableStateVar)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT as GLUT
import qualified Client as Client
import qualified PQueue as PQ
import qualified Events as Events
import Control.Concurrent.STM
import System.Environment
 
main :: IO ()
main = 
    do  args <- getArgs
        let ip = args !! 0
        let priorities = args !! 1
        (progname, _) <- GLUT.getArgsAndInitialize
        (outqueue, inqueue) <- Client.client (Just ip) priorities
        GLUT.createWindow "Hello World"
        GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBAMode]
        GLUT.keyboardMouseCallback $=! Just (keyboardMouse inqueue outqueue)
        GLUT.displayCallback $=! render inqueue
        atomically $ PQ.writeThing inqueue (Events.lookupPriority Events.Init) Events.Init
        GLUT.mainLoop

render pqueue =
    do  event <- atomically $
            do  e <- PQ.getThing pqueue
                case e of
                    Nothing -> retry
                    Just event -> return event
        putStrLn $ "Got event"
        (Events.lookupHandler event Events.Client) event
        GL.flush
        GLUT.swapBuffers

keyboardMouse iqueue oqueue (GLUT.MouseButton GLUT.LeftButton) GLUT.Down _ (GLUT.Position x y) =
    do  (p1, p2) <- normalizeXY (fromIntegral x) (fromIntegral y)
        let event = Events.Click p1 p2
        putStrLn $ "KeyboardMouse callback"
        atomically $ PQ.writeThing oqueue (Events.lookupPriority event) event
        atomically $ PQ.writeThing iqueue (Events.lookupPriority event) event
        putStrLn $ "wrote things"
        --GL.preservingMatrix $ GL.renderPrimitive GL.Points $ do
        --    GL.color (GL.Color4 0 1 0 1 :: GL.Color4 Float)
        --    GL.vertex (GL.Vertex2 p1 p2 :: GL.Vertex2 Float)
        --GL.flush
        --GLUT.swapBuffers
keyboardMouse _ _ _ _ _ _ = return ()

normalizeXY x y = 
    do  (_, (GLUT.Size w h)) <- GLUT.get GLUT.viewport
        return (x/(fromIntegral w), y/(fromIntegral h))