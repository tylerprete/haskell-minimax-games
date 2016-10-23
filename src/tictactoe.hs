-- Tyler Prete
-- TicTacToe using Minimax library

module TicTacToe
where
import Minimax
import Data.Array.Diff
import Control.Monad

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

data Player = X | O
    deriving (Show, Eq, Ord)
data Board = Board (DiffArray Int (Maybe Player))
    deriving (Show)
data TTTGameState = TTTGameState Board Player
instance GameState TTTGameState where
    terminalState (TTTGameState b _) = terminal b
    evaluateState (TTTGameState b _) = evaluateX b
    genSuccessors (TTTGameState b _) = emptySquares b
    makeSuccessor (TTTGameState b p) index = (TTTGameState (setSquare b index p) (switchPlayer p))
    isMaximizing (TTTGameState _ p) = p == X

type Row = Int
type Col = Int

switchPlayer    :: Player -> Player
switchPlayer X = O
switchPlayer O = X

terminal    :: Board -> Bool
terminal b = case (boardWinner b) of
        Nothing -> boardFull b
        _ -> True

evaluate    :: Board -> Int -> Int -> Int -> Int
evaluate b noVal xVal oVal = case (boardWinner b) of
                Nothing -> noVal
                (Just X) -> xVal
                (Just O) -> oVal

evaluateX    :: Board -> Int
evaluateX b = evaluate b 0 1 (-1)

evaluateO    :: Board -> Int
evaluateO b = evaluate b 0 (-1) 1

msame    :: Ord a => Maybe a -> Maybe a -> Maybe a
msame a@(Just x) b@(Just y) | x == y = a
msame _ _ = Nothing

boardSearch    :: Board -> Int -> Int -> (Int -> Int) -> Maybe Player
boardSearch (Board b) index stop _ | index == stop = b ! index
boardSearch brd@(Board b) index stop next =
    (b ! index) `msame` boardSearch brd (next index) stop next

rowWinner   :: Board -> Row -> Maybe Player
rowWinner b r = boardSearch b start stop (+1)
    where   start = (r * 3)
            stop = start + 2

colWinner   :: Board -> Col -> Maybe Player
colWinner b c = boardSearch b start stop (+3)
    where   start = c
            stop = start + 6

diagWinnerTL    :: Board -> Maybe Player
diagWinnerTL b = boardSearch b 0 8 (+4)

diagWinnerTR    :: Board -> Maybe Player
diagWinnerTR b = boardSearch b 2 6 (+2)

diagWinner    :: Board -> Maybe Player
diagWinner b = diagWinnerTL b `mplus` diagWinnerTR b

allRowWinner    :: Board -> Maybe Player
allRowWinner b = foldr mplus Nothing (map (\r -> rowWinner b r) [0,1,2])

allColWinner    :: Board -> Maybe Player
allColWinner b = foldr mplus Nothing (map (\c -> colWinner b c) [0,1,2])

boardWinner    :: Board -> Maybe Player
boardWinner b = diagWinner b `mplus` allRowWinner b `mplus` allColWinner b

emptyBoard    :: Board
emptyBoard = Board $ listArray (0,8) [Nothing | i <- [0..8]]

setSquare    :: Board -> Int -> Player -> Board
setSquare (Board b) index p = (Board (b // [(index, (Just p))]))

emptySquares    :: Board -> [Int]
emptySquares (Board b) = filter (\ind -> b ! ind == Nothing) [0..8]

boardFull    :: Board -> Bool
boardFull b = null $ emptySquares b

makeMove    :: (Int, Maybe Int)
makeMove = minimax (TTTGameState emptyBoard X) False 0 10

makeMoveGS gs = let (score, move) = alphabeta gs 0 10 (-100) 100 in
    case move of
        (Just x) -> makeSuccessor gs x
        Nothing -> error "should never get Nothing as a move"
