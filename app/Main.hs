import Network.Socket
import Lib 

port :: PortNumber
port = 1234

main :: IO ()
main = do
  putStrLn "hello from server!"
  runServer port
