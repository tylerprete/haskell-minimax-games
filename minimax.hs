module Minimax
where

minimax	::	a -> Bool -> (a -> Bool -> [a]) -> (a -> Int) -> (a -> Bool) -> Int
minimax gameState _ _ evaluate terminal | terminal gameState = evaluate gameState
minimax gameState maximize genSuccessors ev term =
	minOrMax $ map (\gs -> minimax gs (not maximize) genSuccessors ev term) (genSuccessors gameState maximize)
	where minOrMax = if maximize then maximum else minimum
