module Main
where
import Minimax
import Kalah
-- Haskell Imports
import System.Exit
import Data.List
import Data.Array.Diff
import System.IO
import Text.Printf

indent = replicate 8 ' '

stringToMove    :: Player -> String -> Int
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
stringToMove _ (s:_) | s == 'Q' || s == 'q' = -999
stringToMove _ _ = -1

moveToString    :: Int -> String
moveToString i = case i of
    0 -> "f"
    1 -> "e"
    2 -> "d"
    3 -> "c"
    4 -> "b"
    5 -> "a"
    7 -> "F"
    8 -> "E"
    9 -> "D"
    10 -> "C"
    11 -> "B"
    12 -> "A"
    otherwise -> error "Bad Move"

getMove :: Player -> IO Int
getMove p =     do
        str <- getLine
        num <- (return $ stringToMove p str)
        case num of
            (-1) -> do { putStr "Invalid Move. Try again: "; hFlush stdout; getMove p }
            (-999) -> do { putStrLn "Quitting Game."; exitWith ExitSuccess }
            num -> return num

printPlayer    :: Player -> IO ()
printPlayer P1 = putStrLn "Player p's move: "
printPlayer P2 = putStrLn "Player P's move: "

stringRow    :: Board -> [Int] -> IO String
stringRow (Board b) xs = do
                lineStr <- return (foldl (\str n -> str ++ (printf "%3d" n)) "" (map (\i -> b ! i) xs))
                return lineStr 

printTopRow    :: Board -> IO ()
printTopRow b = do  str <- stringRow b [12,11..7]
                    putStrLn $ indent ++ "P" ++ str

printBottomRow  :: Board -> IO ()
printBottomRow b = do   str <- stringRow b [0..5]
                        putStrLn $ indent ++ " " ++ str ++ "  p"


printKalahs    :: Board -> IO ()
printKalahs (Board b) = putStrLn $ indent ++ (show $ b ! 13) ++ (replicate 20 ' ') ++ (show $ b ! 6)

printBoard    :: Board -> IO ()
printBoard b = do
    printTopRow b
    printKalahs b
    printBottomRow b


printGameState    :: KalahGameState -> IO ()
printGameState (KalahGameState b p _) = do
    printPlayer p
    printBoard b

initialGameState = KalahGameState initialBoard P1 P1

-- applyMove    :: KalahGameState -> Int -> KalahGameState
applyMove gs m = return (sow gs m) >>= playGame

humanMoveGS (KalahGameState _ p _) = do m <- getMove p; putStrLn ""; return m

winner    :: Player -> String
winner p = "Winner is " ++ (show p)

makeMoveGS  :: KalahGameState -> IO Int
makeMoveGS gs = do  (score, move) <- return $ alphabeta gs 0 8 (-10000) 10000 
                    case move of
                        (Just x) -> do { printf "Selected machine move is '%s'. (evaluation = %d)\n\n" (moveToString x) score; return x }
                        Nothing -> error "Should never get nothing as a move"


playGame gs@(KalahGameState b p p2) | terminal b = 
    let winString = case (evaluateState gs) of
              x | x > 0 -> winner p2
              x | x < 0 -> winner (switchPlayer p2)
              0 -> "Draw."
    in (putStrLn $ "Game over. " ++ winString) >> printGameState gs

playGame gs@(KalahGameState b P1 P1) = printGameState gs >> putStrLn "Machine moving" >> makeMoveGS gs >>= \m -> applyMove gs m
playGame gs@(KalahGameState b P2 _) = printGameState gs >> putStr "Enter move: " >> hFlush stdout >> humanMoveGS gs >>= \m -> applyMove gs m

startGameState = KalahGameState initialBoard P1 P1
main = do 
    startGS <- return startGameState
    playGame startGS
