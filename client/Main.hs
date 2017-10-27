import Network.Socket
import System.IO
import Conf (defaultPort)
import TTTParser
import NetworkTTT
import TicTacTow
import System.Random

clientPlayer = X

firstMove = "-X-|---|---"
startGrid = strToGame firstMove
    

main :: IO ()
main = do  
  handle <- makeHandle

  startPlayI <- randomRIO (0, 8 :: Int)
  putStrLn $ "stating pos: " ++ show startPlayI
  
  play handle startGrid


play :: Handle -> NetworkGame -> IO ()
play handle game = do
  putStrLn $ "play...?"
  putGrid' game

  _ <- getLine
  
  postMove handle game
  
  gridE <- getGrid handle
  
  case gridE of
    Left err   -> putStrLn err
    Right grid -> play handle (makeMove grid clientPlayer)


makeHandle :: IO Handle
makeHandle = do
  sock <- socket AF_INET Stream 0
  localHost <- inet_addr "127.0.0.1"

  let serverAddr = SockAddrInet defaultPort localHost
  connect sock serverAddr

  socketToHandle sock ReadWriteMode

