module Main
where
import TicTacToe
import System.CPUTime

main = do 	startTime <- getCPUTime
		print makeMove
		endTime <- getCPUTime
		timeDiff <- return $ (endTime - startTime) `div` 1000000000000
		putStrLn $ "Full gameState calculation took " ++ (show timeDiff) ++ " seconds."
