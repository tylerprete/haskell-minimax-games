module Minimax
where

minimax	::	a -> Bool -> (a -> Bool -> [a]) -> (a -> Int) -> (a -> Bool) -> Int
minimax gameState _ _ evaluate terminal | terminal gameState = evaluate gameState
minimax gameState minimize genSuccessors ev term =
	minOrMax $ map (\gs -> minimax gs (not minimize) genSuccessors ev term) (genSuccessors gameState minimize)
	where minOrMax = if minimize then minimum else maximum
