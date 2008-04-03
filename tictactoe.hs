-- Tyler Prete
-- TicTacToe using Minimax library

module Main
where
import Minimax
import Data.Array.Diff
import Monad

-- Board is as follows
--    0 1 2
--   _______
-- 0 |0|1|2|
--   _______   
-- 1 |3|4|5|
--   _______
-- 2 |6|7|8|
-- col's horizontal
-- row's vertical
-- Numbers inside actual index of array 

data Player = X | Y
	deriving (Show, Eq)
data Board = Board (DiffArray Int (Maybe Player))
	deriving (Show)
type Row = Int
type Col = Int

terminal	:: Board -> Bool
terminal b = case (boardWinner b) of
		Nothing -> False
		_ -> True

evaluate	:: Board -> Int -> Int -> Int -> Int
evaluate b noVal xVal yVal = case (boardWinner b) of
				Nothing -> noVal
				(Just X) -> xVal
				(Just Y) -> yVal

evaluateX	:: Board -> Int
evaluateX b = evaluate b 0 1 (-1)

evaluateY	:: Board -> Int
evaluateY b = evaluate b 0 (-1) 1

boardSearch	:: Board -> Int -> Int -> (Int -> Int) -> Maybe Player
boardSearch (Board b) index stop _ | index == stop = b ! index
boardSearch brd@(Board b) index stop next = 
	(b ! index) `mplus` boardSearch brd (next index) stop next

rowWinner	:: Board -> Row -> Maybe Player
rowWinner b r = boardSearch b start stop (+1)
	where	start = (r * 3)
		stop = start + 2

colWinner	:: Board -> Col -> Maybe Player
colWinner b c = boardSearch b start stop (+3)
	where	start = c
		stop = start + 6

diagWinnerTL	:: Board -> Maybe Player
diagWinnerTL b = boardSearch b 0 8 (+4)

diagWinnerTR	:: Board -> Maybe Player
diagWinnerTR b = boardSearch b 2 6 (+2) 

diagWinner	:: Board -> Maybe Player
diagWinner b = diagWinnerTL b `mplus` diagWinnerTR b

allRowWinner	:: Board -> Maybe Player
allRowWinner b = foldr mplus Nothing (map (\r -> rowWinner b r) [0,1,2])

allColWinner	:: Board -> Maybe Player
allColWinner b = foldr mplus Nothing (map (\c -> colWinner b c) [0,1,2])

boardWinner	:: Board -> Maybe Player
boardWinner b = diagWinner b `mplus` allRowWinner b `mplus` allColWinner b

emptyBoard	:: Board
emptyBoard = Board $ listArray (0,8) [Nothing | i <- [0..8]]

setSquare	:: Board -> Int -> Player -> Board
setSquare (Board b) index p = (Board (b // [(index, (Just p))]))

generateSuccessors	:: Board -> Bool -> [Board]
generateSuccessors b xMove = map (\i -> setSquare b i p) [0..8]
	where p = if xMove then X else Y

playGame	:: Int
playGame = minimax emptyBoard True generateSuccessors evaluateX terminal
main =
	print playGame
