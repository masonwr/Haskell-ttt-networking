import Network.Socket
import System.IO

port = 1234

firstMove = "X--|---|---"

main :: IO ()
main = do  
  handle <- makeHandle
  move firstMove handle
  

move :: String -> Handle -> IO ()
move m h = do
  hPutStrLn h m
  res <- hGetLine h
  putStrLn $ "res: " ++ res
  res <- getLine
  move res h
  



makeHandle :: IO Handle
makeHandle = do
  sock <- socket AF_INET Stream 0
  localHost <- inet_addr "127.0.0.1"

  let serverAddr = SockAddrInet port localHost
  connect sock serverAddr

  socketToHandle sock ReadWriteMode

