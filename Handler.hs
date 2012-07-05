module Handler where

import Listener

--listenerHandlers = [(keyPress, [keyPressHandler])]

lookupHandler (KeyPress _) = keyPressHandler

keyPressHandler (KeyPress c) = putStrLn $ "keypress: " ++ show c