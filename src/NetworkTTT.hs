module NetworkTTT ( getGrid
                  , postMove
                  , NetworkGame
                  , strToGame
                  , makeMove
                  , putGrid'
                  ) where

import System.IO
import TTTParser 
import TicTacTow



type NetworkGame = Either String Grid



putGrid' :: NetworkGame -> IO ()
putGrid' g =
  case g of
    Left msg -> putStrLn msg
    Right g  -> putGrid g



getGrid :: Handle -> IO NetworkGame
getGrid handle = do
  res <- hGetLine handle  
  pure $ strToGame res  


    
strToGame :: String -> NetworkGame
strToGame gridStr =  case parseGame gridStr of
    Just grid -> Right grid
    Nothing   -> Left gridStr



makeMove :: Grid -> Player -> NetworkGame
makeMove g p
  | wins O g = Left "O wins"
  | wins X g = Left "X wins"
  | isFull g = Left "tie"
  | otherwise = Right $ bestmove g p



postMove :: Handle -> NetworkGame -> IO ()
postMove h game =
  case game of
    Left msg -> hPutStrLn h msg
    Right g  -> hPutStrLn h $ gridToStr g  
