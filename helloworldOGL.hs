import Graphics.Rendering.OpenGL.GL (($=), ($=!), makeGettableStateVar)
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLUT as GLUT
import qualified Client as Client
import qualified PQueue as PQ
import qualified Events as Events
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
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

render = GLUT.swapBuffers

checkEvents pqueue = forever $
    do  event <- atomically $
            do  e <- PQ.getThing pqueue
                case e of
                    Nothing -> retry
                    Just event -> return event
        putStrLn $ "Got event"
        (Events.lookupHandler event Events.Client) event
        GLUT.postRedisplay Nothing

keyboardMouse iqueue oqueue (GLUT.MouseButton GLUT.LeftButton) GLUT.Down _ (GLUT.Position x y) =
    do  (p1, p2) <- normalizeXY (fromIntegral x) (fromIntegral y)
        putStrLn $ "click coords: " ++ show x ++ ", " ++ show y
        putStrLn $ "normalized: " ++ show p1 ++ ", " ++ show p2
        let event = Events.Click p1 p2
        putStrLn $ "KeyboardMouse callback"
        atomically $ PQ.writeThing oqueue (Events.lookupPriority event) event
        atomically $ PQ.writeThing iqueue (Events.lookupPriority event) event
        putStrLn $ "wrote things"
keyboardMouse _ _ _ _ _ _ = return ()

normalizeXY x y = 
    do  (_, (GLUT.Size width height)) <- GLUT.get GLUT.viewport
        let w = fromIntegral width
        let h = fromIntegral height
        return ((x-w/2.0)/(w/2.0), (y-h/2.0)/((-h)/2.0))