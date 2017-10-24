
import Conf (defaultPort)

import Control.Concurrent
import Network.Socket
import System.IO
import TicTacTow (putGridx)
import TTTParser (parseGame)


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
  res <- stripnr <$> hGetLine handle

  putStrLn "res!"
  
  case parseGame res of
    Just game -> do
      putGrid game
      hPutStrLn handle "got game!"
      runGame handle
    Nothing   -> do
      putStrLn "No game"


-- echo :: Handle -> IO ()
-- echo handle = do
--   line <- stripnr <$> hGetLine handle

--   case parseGame line of
--     Just game -> putGrid game
--     Nothing -> putStrLn $ "No Game: " ++ line

   
--   if line == "exit"
--     then hClose handle
--     else do    
--     hPutStrLn handle ("--" ++ line ++ "--")
--     echo handle
