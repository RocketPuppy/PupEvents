module Listener where

import GHC.IO.Handle
import Control.Concurrent.STM.TChan
import Text.Parsec

listenerParsers = [keyPress]

data EventParser = EventParser (Parsec String () Event)

-- Internal Event Representation
data Event = KeyPress Char | MousePress Int

-- list of some events
-- KeyPress
keyPress = EventParser p
    where p =   (do string "KeyPress"
                    char '\0'
                    c <- option '\0' (noneOf "\0")
                    return (KeyPress c))