import Network.Socket
import System.IO
import System.Environment
import Conf (defaultPort)
import TTTParser
import NetworkTTT
import TicTacTow
import System.Random
import Data.Maybe

clientPlayer :: Player
clientPlayer = X


localHostIP :: String
localHostIP = "127.0.0.1"


main :: IO ()
main = do
  args <- getArgs

  let ipAddr = (head $ args ++ [localHostIP])
  
  putStrLn $ "Connecting too: " ++ ipAddr  
  
  handle <- makeHandle ipAddr
  startGrid <- getRandomStartingGrid
  
  play handle (gridToNG startGrid)


getRandomStartingGrid :: IO Grid
getRandomStartingGrid = do
  startPlayI <- randomRIO (0, 8 :: Int)
  pure . fromJust $ move empty startPlayI clientPlayer


play :: Handle -> NetworkGame -> IO ()
play handle game = do
  putStrLn $ "play...?"
  putGrid' game

  --_ <- getLine
  
  postMove handle game  
  gridE <- getGrid handle
  
  case gridE of
    Left err   -> putStrLn err
    Right grid -> play handle (makeMove grid clientPlayer)


makeHandle :: String -> IO Handle
makeHandle ipStr = do
  sock <- socket AF_INET Stream 0
  localHost <- inet_addr ipStr

  let serverAddr = SockAddrInet defaultPort localHost
  connect sock serverAddr

  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering

  return handle

