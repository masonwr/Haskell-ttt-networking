
import Conf (defaultPort)

import Control.Concurrent
import Network.Socket
import System.IO
import TicTacTow (putGrid)
import TTTParser (parseGame)



main :: IO ()
main = do
  putStrLn "hello from server!"
  runServer defaultPort


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
  echo handle



echo :: Handle -> IO ()
echo handle = do
  line <- stripnr <$> hGetLine handle

  case parseGame line of
    Just game -> putGrid game
    Nothing -> putStrLn "No Game"

  print line
  print $ stripr line  
  if line == "exit"
    then hClose handle
    else do    
    hPutStrLn handle ("--" ++ line ++ "--")
    echo handle
