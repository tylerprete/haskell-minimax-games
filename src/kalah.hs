-- Tyler Prete

module Kalah
where

import Minimax
-- Haskell imports
import Data.List
import Data.Array.Diff

data Player = P1 | P2
    deriving (Eq, Show)

type Stones = Int

data Board = Board (DiffUArray Int Int)
    deriving (Show)

holeCount = 6

initialBoard = Board $ listArray (0,13) (cycle [6,6,6,6,6,6,0])

data KalahGameState = KalahGameState Board Player Player
    deriving (Show)
instance GameState KalahGameState where
    terminalState (KalahGameState b _ _) = bothKalahTotal b == 72
    evaluateState (KalahGameState b _ p2) = kalahAdvantage b p2
    genSuccessors (KalahGameState b p _) = possibleMoves b p
    makeSuccessor = sow
    isMaximizing (KalahGameState _ p p2) = p == p2

switchPlayer    :: Player -> Player
switchPlayer P1 = P2
switchPlayer P2 = P1

rowTotal    :: Board -> Player -> Int
rowTotal (Board b) p = foldr (+) 0 $ map (b!) (if p == P1 then [0..5] else [7..12])

rowEmpty    :: Board -> Player -> Bool
--rowEmpty (Board b) p = rowEmptyHelper b (if p == P1 then [0..5] else [7..12])
--    where     rowEmptyHelper b [] = True
--        rowEmptyHelper b (x:xs) | (b ! x) /= 0 = False
--                    | otherwise = rowEmptyHelper b xs
rowEmpty (Board b) p = null $ filter (\i -> (b ! i) /= 0) (if p == P1 then [0..5] else [7..12])

kalahTotal    :: Board -> Player -> Int
kalahTotal (Board b) p = b ! (kalahPos p)

bothKalahTotal    :: Board -> Int
bothKalahTotal b = kalahTotal b P1 + kalahTotal b P2

kalahPos    :: Player -> Int
kalahPos p = if p == P1 then 6 else 13

terminal    :: Board -> Bool
terminal b = rowEmpty b P1 || rowEmpty b P2

kalahAdvantage    :: Board -> Player -> Int
kalahAdvantage b p = (if terminal b then 100 else 1) * (kalahTotal b p - kalahTotal b (switchPlayer p))

possibleMoves    :: Board -> Player -> [Int]
possibleMoves (Board b) p = filter (\i -> (b ! i) > 0) (if p == P1 then [0..5] else [7..12])

sow :: KalahGameState -> Int -> KalahGameState
sow (KalahGameState brd@(Board b) p p2) pos = (KalahGameState newBoard newP p2)
    where   count = (b ! pos)
            newB = Board (b // [(pos, 0)])
            (newBrd, newP) = placeStones newB p (pos + 1) count
            newBoard = if terminal newBrd then endGameMove newBrd else newBrd

moveHoleToKalah    :: Board -> Int -> Board
moveHoleToKalah (Board b) i = Board $ b // [(k, curr + val), (i, 0)] where
    k = if i < 6 then 6 else 13
    curr = (b ! k)
    val = (b ! i)

endGameMove    :: Board -> Board
--endGameMove b = foldr (\i brd -> moveHoleToKalah brd i) b ([0..5] ++ [7..12])
endGameMove (Board b) = Board $ b // ([(6, (b ! 6) + p1Total), (13, (b ! 13) + p2Total)] ++ p1Zeros ++ p2Zeros) where
    totalFunc = \l -> sum $ map (\i -> b ! i) l
    p1Total = totalFunc [0..5]
    p2Total = totalFunc [7..12]
    zeroFunc = map (\i -> (i,0))
    p1Zeros = zeroFunc [0..5]
    p2Zeros = zeroFunc [7..12]

nextPos    :: Player -> Int -> Int
nextPos P1 pos | pos == 12 = 0
nextPos P2 pos | pos == 5 = 7
nextPos _ pos | pos == 13 = 0
nextPos _ pos = pos + 1

holeOwner    :: Int -> Player
holeOwner n | n >= 0 && n <= 6 = P1
holeOwner n | n >= 7 && n <= 13 = P2

holeAcrossBoard    :: Int -> Int
holeAcrossBoard = (-) 12

placeStones    :: Board -> Player -> Int -> Int -> (Board, Player)
{-placeStones b p _ 0 = (b, switchPlayer p)
placeStones (Board b) p pos 1 | pos == kalahPos p = (newBoard, p)
    where    newBoard = Board $ b // [(pos, (b ! pos) + 1)]
placeStones brd@(Board b) p pos 1 | b ! pos == 0 && b ! (holeAcrossBoard pos) /= 0 && holeOwner pos == p =
    (Board $ b // [(otherHole, 0), (kalahPos p, newCount)], switchPlayer p)
    where   otherHole = holeAcrossBoard pos
	        otherHoleCount = (b ! otherHole)
	        newCount = (kalahTotal brd p) + otherHoleCount + 1

placeStones (Board b) p pos count = placeStones (Board (b // [(pos, (b!pos)+1)])) p (nextPos p pos) (count - 1)
-}

placeLastStone  :: Board -> Player -> Int -> (Board, Player)
placeLastStone brd@(Board b) p pos | pos /= 6 && pos /= 13 && b ! pos == 0 && holeOwner pos == p =
    (Board $ b // [(otherHole, 0), (kalahPos p, newCount)], switchPlayer p)
    where   otherHole = holeAcrossBoard pos
            otherHoleCount = (b ! otherHole)
            newCount = (kalahTotal brd p) + otherHoleCount + 1
placeLastStone (Board b) p pos = (newBoard, if pos == kalahPos p then p else switchPlayer p)
    where   newBoard = Board $ b // [(pos, (b ! pos) + 1)]

takeReverse    :: [Int] -> Int -> [Int]
takeReverse l count = takeReverse' l count [] where
    takeReverse' (x:_) 1 acc = x:acc
    takeReverse' (x:xs) count acc = takeReverse' xs (count - 1) (x:acc)

placeStones (Board b) p pos count = placeLastStone (Board $ b // updates) p newPos where
    allUpdates = iterate (\i -> nextPos p i) pos
    currUpdates = takeReverse allUpdates count
    updates = map (\l -> (head l, (b ! head l) + length l)) $ group . sort $ tail currUpdates
    newPos = head currUpdates
