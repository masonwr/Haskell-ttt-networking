module TTTParser ( parseGame
                 , gridToStr
                 ) where 

import Conf (defaultGameSize)
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
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
parseRow = count defaultGameSize parsePlayer


parseGrid :: Parser Grid
parseGrid = sepBy parseRow (char '|')

trimGridToSize :: Grid -> Maybe Grid
trimGridToSize g = if length g < defaultGameSize
                   then Nothing
                   else pure $ take defaultGameSize g

parseGame :: String -> Maybe Grid
parseGame gameStr =
  case parse parseGrid "grid parser" gameStr of
    Right game -> trimGridToSize game
    Left _ -> Nothing
  


gridToStr :: Grid -> String
gridToStr = join . join . intersperse ["|"] . map (map show)
