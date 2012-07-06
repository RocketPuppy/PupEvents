module Client where

import Network.Socket
import System.IO
import Control.Monad

main =
    do  addrinfos <- getAddrInfo Nothing (Just "72.192.13.69") (Just "1267")
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream 6
        setSocketOption sock KeepAlive 1
        connect sock (addrAddress serveraddr)
        handle <- socketToHandle sock WriteMode
        hSetBuffering handle (BlockBuffering Nothing)
        acceptInput handle

acceptInput handle = forever $
    do  char <- getChar
        hPutStr handle ("KeyPress\0" ++ char:"\0\0")
        hFlush handle