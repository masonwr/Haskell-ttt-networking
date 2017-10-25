module NetworkTTT (makePlay, readNextMove) where

import System.IO
import TTTParser 
import TicTacTow



readNextMove :: Handle -> IO (Either String Grid)
readNextMove handle = do
  res <- hGetLine handle

  return $ case parseGame res of
    Just grid -> Right grid
    Nothing   -> Left "Parse Error"
    

networkMove :: Grid -> Player -> Either String Grid
networkMove g p
  | wins O g = Left "O wins"
  | wins X g = Left "X wins"
  | isFull g = Left "tie"
  | otherwise = Right $ bestmove g p


makePlay :: Player -> Maybe Grid -> Maybe String
makePlay p gm = do
  g <- gm
  pure $ case networkMove g p of
    Left msg -> msg
    Right g  -> gridToStr g
