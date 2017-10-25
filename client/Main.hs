import Network.Socket
import System.IO
import Conf (defaultPort)
import TTTParser
import NetworkTTT
import TicTacTow

firstMove :: IO String
firstMove = pure "---|O--|---"

main :: IO ()
main = do  
  handle <- makeHandle
  start <- firstMove
  play handle start
  

play :: Handle -> String -> IO ()
play handle move = do
  putStrLn $ "send?  " ++ move
  _ <- getLine -- return

  hPutStrLn handle move

  res <- hGetLine handle
  
  putStrLn $ "responce from server: " ++ res

  case makePlay O (parseGame res) of
    Just nextMove -> play handle nextMove
    Nothing -> putStrLn "Error!"
      


makeHandle :: IO Handle
makeHandle = do
  sock <- socket AF_INET Stream 0
  localHost <- inet_addr "127.0.0.1"

  let serverAddr = SockAddrInet defaultPort localHost
  connect sock serverAddr

  socketToHandle sock ReadWriteMode

