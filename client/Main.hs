import Network.Socket
import System.IO
import Conf (defaultPort)


firstMove :: IO String
firstMove = pure "X--|---|---"

main :: IO ()
main = do  
  handle <- makeHandle
  start <- firstMove
  echo handle
  


play :: Handle -> String -> IO ()
play handle move = do
  hPutStrLn handle move
  res <- hGetLine handle
  putStrLn $ "res: " ++ res
  _ <- getLine -- return
  play handle res
  

echo :: Handle -> IO ()
echo h = do
  
  res <- getLine
  hPutStrLn h res

  fromServer <- hGetLine h
  putStrLn $ "> " ++ fromServer
  
  echo h  


makeHandle :: IO Handle
makeHandle = do
  sock <- socket AF_INET Stream 0
  localHost <- inet_addr "127.0.0.1"

  let serverAddr = SockAddrInet defaultPort localHost
  connect sock serverAddr

  socketToHandle sock ReadWriteMode

