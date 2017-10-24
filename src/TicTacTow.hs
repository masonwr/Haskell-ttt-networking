module TicTacTow where

import Conf (defaultGameSize)
import Data.Char
import Data.Maybe
import Data.List
import System.IO

size :: Int
size = defaultGameSize

data Player = O | B | X
  deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

isFull :: Grid -> Bool
isFull = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs
         then O else X
  where os = length (filter (== O) ps)
        xs = length (filter (== X) ps)
        ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where line = all (==p)
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

diag :: [[a]] -> [a]
diag g = [g !! n !! n | n <- [0..size-1]]  
        
won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']


showRow :: [Player] -> [String]
showRow =  beside . interleave bar . map showPlayer
  where beside = foldr1 (zipWith (++))
        bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]


interleave :: a -> [a] -> [a]
interleave x []  = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys


valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B


chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid Number!"
      getNat prompt


tictactow :: IO ()
tictactow = run empty O

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1,1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins\n"
         | wins X g = putStrLn "Player X wins\n"
         | isFull g = putStrLn "DRAW"
         | otherwise = do
             i <- getNat (prompt p)
             case move g i p of
               Nothing -> do
                 putStrLn "Error: Invalid move (return to continue)"
                 _ <- getLine
                 run g p                                  
               Just g' -> run g' (next p)


prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move:"

--

data Tree a = Node a [Tree a] deriving (Eq, Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <-  moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | isFull g  = []  
  | otherwise = mapMaybe (\i -> move g i p) [0..((size^2)-1)]

          
move :: Grid -> Int -> Player -> Maybe Grid
move g i p = if valid g i
                then Just $  chop size (xs ++ [p] ++ ys)
                else Nothing
  where (xs, B:ys) = splitAt i (concat g)


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []

minimax (Node g ts) =
  case turn g of
    O -> Node (g, minimum ps) ts'
    X -> Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps  = [ p | Node (_, p) _ <- ts' ]

bestmove :: Grid -> Player -> Grid
bestmove g p = head [ g' | Node (g', p') _ <- ts, p' == best ]
  where tree = prune depth (gametree g p)
        Node (_, best) ts = minimax tree


    
-- mainz

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1,1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "O wins"
  | wins X g = putStrLn "X wins"
  | isFull g   = putStrLn "It's a draw!\n"
  | p == O   = do
      i <- getNat (prompt p)
      case move g i p of
        Nothing -> putStrLn "ERROR: Invalid Move!"
        Just g' -> play g' (next p)
  | p == X   = do
      putStrLn "Player X is thinking..."
      (play $! (bestmove g p)) (next p)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O
