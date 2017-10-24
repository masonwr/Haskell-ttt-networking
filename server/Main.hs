import Network.Socket
import Conf (defaultPort)
import Lib 



main :: IO ()
main = do
  putStrLn "hello from server!"
  runServer defaultPort
