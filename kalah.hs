-- Tyler Prete

module Kalah
where

import Minimax
import Data.Array.Diff

data Player = P1 | P2

type Stones = Int

data Board = Board (DiffArray Int Int)
	deriving (Show)

data KalahGameState = KalahGameState Board Player Player
instance GameState KalahGameState where
	terminalState (KalahGameState b _ _) = terminal b
	evaluateState (KalahGameState b p p2) = kalahTotal p2
	genSuccessors (KalahGameState b _ _) = possibleMoves b
	makeSuccessor gs index = sow gs index

switchPlayer	:: Player -> Player
switchPlayer P1 = P2
switchPlayer P2 = P1

rowTotal	:: Board -> Player -> Int
rowTotal (Board b) p = foldr (+) 0 (b!) (if p == P1 then [0..5] else [7..12])

kalahTotal	:: Board -> Player -> Int
kalahTotal (Board b) P1 = b ! 6
kalahTotal (Board b) P2 = b ! 13

terminal	:: Board -> Bool
terminal b = kalahTotal b P1 == 0 || kalahTotal b P2 == 0

kalahAdvantage	:: Board -> Player -> Int
kalahAdvantage (Board b) p = (if terminal b then 100 else 1) * (kalahTotal p - kalahTotal (switchPlayer p))

possibleMoves	:: Board -> Player -> Int
possibleMoves (Board b) p = filter (0 < b!) (if p == P1 then [0..5] else [7..12])

sow	:: (KalahGameState a) => a -> Int -> a
sow (_ p _) =  
