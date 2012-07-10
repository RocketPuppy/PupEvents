module Events where

import GHC.IO.Handle
import Control.Concurrent.STM.TChan
import Text.Parsec
import PQueue

data EventPackage = EventPackage EventPrinter EventParser

-- for getting from the wire
data EventParser = EventParser (Parsec String () Event)

-- for printing on the wire
data EventPrinter = EventPrinter (Event -> String)

-- Internal Event Representation
data Event = KeyPress Char 
    deriving (Show)

data HandlerMode = Client | Server
-- list of some events
parsers = [keyPress]
-- KeyPress
unKeyPress (KeyPress c) = "KeyPress\0" ++ c:"\0\0"

keyPress :: Parsec String () Event
keyPress = do string "KeyPress"
              char '\0'
              c <- option '\0' (noneOf "\0")
              return (KeyPress c)

keyPressHandler Server (KeyPress c) = putStrLn $ "keypress from client: " ++ show c
keyPressHandler Client (KeyPress c) = putStrLn $ "keypress from server: " ++ show c

lookupPriority (KeyPress _) = 0
lookupHandler (KeyPress _) mode = keyPressHandler mode
lookupParser (KeyPress _) = keyPress
lookupUnHandler (KeyPress _) = unKeyPress
