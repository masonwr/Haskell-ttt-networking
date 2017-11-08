
import Conf (defaultPort)

import Control.Concurrent
import Network.Socket
import System.IO
import TicTacTow (putGrid, bestmove, Player(..), Grid)
import TTTParser (parseGame, gridToStr)
import NetworkTTT



serverPlayer :: Player
serverPlayer = O

-- Main 
main :: IO ()
main = do
  putStrLn "hello from server!"
  runServer defaultPort


-- Set up listening socket and run game loop
runServer :: PortNumber -> IO ()
runServer port = do
  -- create a TCP IPv4 socket
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1

  -- bind socket to port (707)
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  
  mainLoop sock



mainLoop :: Socket -> IO ()
mainLoop sock = do
  -- wait for connections
  conn <- accept sock

  -- fork new IO thread to handle connection
  _ <- forkIO (runConn conn)   
  mainLoop sock


-- handle the newly created connection, hand off to gameloop
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
  print $ "Connection from " ++ (show addr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  runGame handle


-- Run the actual game, with some logging turned on.
runGame :: Handle -> IO ()
runGame handle = do
  netGame <- getGrid handle
  putStrLn $ "received: " ++ show netGame
  
  case netGame of
    Left err   -> do
      putStrLn "closing connection."
      hPutStrLn handle err
      
    Right grid -> do
      putStrLn $ "sending: " ++ show (makeMove grid serverPlayer)
      postMove handle (makeMove grid serverPlayer)
      runGame handle

  

