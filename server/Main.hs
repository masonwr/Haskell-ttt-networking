
import Conf (defaultPort)

import Control.Concurrent
import Network.Socket
import System.IO
import TicTacTow (putGrid, bestmove, Player(..), Grid)
import TTTParser (parseGame, gridToStr)
import NetworkTTT

serverPlayer = X


main :: IO ()
main = do
  putStrLn "hello from server!"
  runServer defaultPort



stripnr :: String -> String
stripnr = filter (/= '\r') . filter (/= '\n')



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
  runGame handle


runGame :: Handle -> IO ()
runGame handle = do
  netGame <- getGrid handle

  putStrLn $ show netGame
  
  case netGame of
    Left err   -> hPutStrLn handle err
    Right grid -> do
      putStrLn $ show (makeMove grid serverPlayer)
      postMove handle (makeMove grid serverPlayer)
      runGame handle

  

