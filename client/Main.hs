import Network.Socket
import System.IO
import System.Environment
import Conf (defaultPort)
import NetworkTTT
import TicTacTow
import System.Random
import Data.Maybe


-- Basic Configuration information
clientPlayer :: Player
clientPlayer = X


localHostIP :: String
localHostIP = "127.0.0.1"



-- Main loop
main :: IO ()
main = do
  -- if no host ip address is specified, local host is assumed.
  args <- getArgs
  let ipAddr = (head $ args ++ [localHostIP])
  
  putStrLn $ "Connecting too: " ++ ipAddr  
  
  handle <- makeHandle ipAddr
  startGrid <- getRandomStartingGrid
  
  play handle (gridToNG startGrid)


-- get a random starting position for the client player.
getRandomStartingGrid :: IO Grid
getRandomStartingGrid = do
  startPlayI <- randomRIO (0, 8 :: Int)
  pure . fromJust $ move empty startPlayI clientPlayer


-- Play loop. If there is an message print to screen,
-- otherwise there is a game, make a play.
play :: Handle -> NetworkGame -> IO ()
play handle game = do
  putStrLn  "play...?"
  putGrid' game
  
  postMove handle game  
  gridE <- getGrid handle
  
  case gridE of
    Left err   -> putStrLn err
    Right grid -> play handle (makeMove grid clientPlayer)


-- Create a socket and wrap it in a IO Handle, a more general IO interface.
makeHandle :: String -> IO Handle
makeHandle ipStr = do
  
  sock <- socket AF_INET Stream 0 -- create a IPv4 TCP socket.  
  localHost <- inet_addr ipStr    -- case ip string into addr

  -- build server networking address, and connect the socket to it.
  let serverAddr = SockAddrInet defaultPort localHost
  connect sock serverAddr

  -- wrap socket in handle, and set buffering rule.
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering

  return handle

