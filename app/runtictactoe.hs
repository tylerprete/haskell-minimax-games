module Main
where
import Minimax
import TicTacToe
import Data.Array.Diff

getNum :: IO Int
getNum = getLine >>= (\numStr -> return (read numStr))

humanMoveGS gs@(TTTGameState b p) = do
            moveInt <- getNum
            newB <- return (setSquare b moveInt p)
            newGS <- return (TTTGameState newB (switchPlayer p))
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
