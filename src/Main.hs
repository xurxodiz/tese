module Main where
  
import Data.List
import IO
import Network
import Control.Concurrent

import Lang

port = 3838

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ messageProcessor handle
    sockHandler sock

messageProcessor :: Handle -> IO ()
messageProcessor h = hGetLine h >>= rail h >> hClose h

rail :: Handle -> String -> IO ()
rail h s = sequence_ . (map $ hPutStrLn h) =<< translate s