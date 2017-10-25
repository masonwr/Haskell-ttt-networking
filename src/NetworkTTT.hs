module NetworkTTT (makePlay) where

import TTTParser 
import TicTacTow 

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
