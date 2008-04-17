module Main
where
import Minimax
import Kalah
-- Haskell Imports
import System
import List
import Data.Array.Diff

indent = replicate 8 ' '

stringToMove	:: Player -> String -> Int
stringToMove P1 "f" = 0
stringToMove P1 "e" = 1
stringToMove P1 "d" = 2
stringToMove P1 "c" = 3
stringToMove P1 "b" = 4
stringToMove P1 "a" = 5
stringToMove P2 "A" = 12
stringToMove P2 "B" = 11
stringToMove P2 "C" = 10
stringToMove P2 "D" = 9
stringToMove P2 "E" = 8
stringToMove P2 "F" = 7
stringToMove _ s | s == "Q" || s == "q" = -999
stringToMove _ _ = -1

getMove :: Player -> IO Int
getMove p = 	do
		str <- getLine
		num <- (return $ stringToMove p str)
		case num of
			(-1) -> do { putStrLn "Invalid Move. Try again: "; getMove p }
			(-999) -> do { putStrLn "Quitting Game."; exitWith ExitSuccess }
			num -> return num

printPlayer	:: Player -> IO ()
printPlayer P1 = putStrLn "Player p's move: "
printPlayer P2 = putStrLn "Player P's move: "

stringRow	:: Board -> [Int] -> IO String
stringRow (Board b) xs = do
				lineStr <- return (intersperse ' ' (foldr (\num str -> (show (b ! num)) ++ str) "" xs))
				return lineStr 

printTopRow	:: Board -> IO ()
printTopRow b = do	str <- stringRow b [12,11..7]
			putStrLn $ indent ++ "P " ++ str

printBottomRow	:: Board -> IO ()
printBottomRow b = do	str <- stringRow b [0..5]
			putStrLn $ indent ++ "  " ++ str ++ " p"

printKalahs	:: Board -> IO ()
printKalahs (Board b) = putStrLn $ indent ++ (show $ b ! 13) ++ (replicate 13 ' ') ++ (show $ b ! 6)

printBoard	:: Board -> IO ()
printBoard b = do
			printTopRow b
			printKalahs b
			printBottomRow b


printGameState	:: KalahGameState -> IO ()
printGameState (KalahGameState b p _) = do
			printPlayer p
			printBoard b

initialGameState = KalahGameState initialBoard P1 P1

{-
humanMoveGS gs@(KalahGameState b p p2) = do
			moveInt <- getMove
			newB <- return (setSquare b moveInt p)
			newGS <- return (KalahGameState newB (switchPlayer p) p2)
			return newGS

printSquare (Board b) ind | (b ! ind) /= Nothing = let (Just x) = (b ! ind) in (show x)
printSquare _ ind = (show ind)

printRow b r = printSquare b (x !! 0) ++ "|" ++ printSquare b (x !! 1) ++ "|"  ++ printSquare b (x !! 2) ++ "\n"
	where x = case r of
			0 -> [0,1,2]
			1 -> [3,4,5]
			2 -> [6,7,8]

spacer = "-----\n"

printBoard b = printRow b 0 ++ spacer ++ printRow b 1 ++ spacer ++ printRow b 2

printGameState gs@(TTTGameState b p) = do
					putStrLn $ (show p) ++ "'s move."
					putStrLn $ printBoard b
					return gs
					

playGame gs@(TTTGameState b p) | terminal b = 
	let winString = case (evaluateState gs) of
			  1 -> "Winner is X"
			  (-1) -> "Winner is O"
			  0 -> "Draw."
	in putStrLn $ "Game over. " ++ winString

playGame gs@(TTTGameState _ X) = putStrLn "Machine moving" >> (return (makeMoveGS gs)) >>= (\newGS -> printGameState newGS) >>= playGame
playGame gs@(TTTGameState _ O) = putStrLn "Your move" >> humanMoveGS gs >>= playGame
main = do 
	startGS <- return (TTTGameState emptyBoard X)
	printGameState startGS
	playGame startGS
-}
