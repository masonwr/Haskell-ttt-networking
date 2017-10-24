{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TTTParser (parseGame) where 

import Text.ParserCombinators.Parsec
--import Text.Parsec
import TicTacTow (Player (..), Grid)
import Data.Char (toUpper, toLower)


char' :: Char -> Parser Char
char' ch = char (toLower ch) <|> char  (toUpper ch)


parsePlayer :: Parser Player
parsePlayer = xp <|> op <|> b
  where xp = char' 'x' *> pure X
        op = char' 'o' *> pure O
        b  = char' '-' *> pure B


parseRow :: Parser [Player]
parseRow = count 3 parsePlayer


parseGrid :: Parser Grid
parseGrid = sepBy parseRow (char '|')


parseGame :: String -> Maybe Grid
parseGame gameStr =
  case parse parseGrid "grid parser" gameStr of
    Right game -> pure game
    Left _ -> Nothing
  


