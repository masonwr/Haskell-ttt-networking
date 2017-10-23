module Lib
    ( runServer    
    ) where

--import Control.Monad
import Control.Concurrent
import Network.Socket
import System.IO


stripr :: String -> String
stripr = filter $ (/= '\r')

stripn :: String -> String
stripn = filter $ (/= '\n')

stripnr :: String -> String
stripnr = stripn . stripr


runServer :: PortNumber -> IO ()
runServer port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  _ <- forkIO (runConn conn)   
  mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
  print $ "Connection from " ++ (show addr)
  handle <- socketToHandle sock ReadWriteMode
  hPutStrLn handle "Hello from haskel"  
  echo handle


echo :: Handle -> IO ()
echo handle = do
  line <- stripnr <$> hGetLine handle
  print line
  print $ stripr line  
  if (line == "exit")
    then hClose handle
    else do    
    hPutStrLn handle $ ("--" ++ line ++ "--")
    echo handle
