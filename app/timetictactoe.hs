module Main
where
import TicTacToe
import Minimax
import System.CPUTime

main = do   startTime <- getCPUTime
            print makeMove
            endTime <- getCPUTime
            timeDiff <- return $ (endTime - startTime) `div` 1000000000000
            putStrLn $ "Full gameState calculation took " ++ (show timeDiff) ++ " seconds."
            gameState <- return (TTTGameState emptyBoard X)
            startTime <- getCPUTime
            print $ alphabeta gameState 0 10 (-100) 100
            endTime <- getCPUTime
            timeDiff <- return $ (endTime - startTime) `div` 1000000000000
            putStrLn $ "Full alpha-beta calculation took " ++ (show timeDiff) ++ " seconds."
