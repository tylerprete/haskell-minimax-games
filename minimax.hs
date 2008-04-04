module Minimax
where
import Debug.Trace

class GameState a where
	evaluateState	:: a -> Int
	terminalState	:: a -> Bool
	genSuccessors	:: a -> [Int]
	makeSuccessor	:: a -> Int -> a

{-minimax	::	a -> Bool -> (a -> Bool -> [a]) -> (a -> Int) -> (a -> Bool) -> Int
minimax gameState _ _ evaluate terminal | terminal gameState = evaluate gameState
minimax gameState minimize genSuccessors ev term =
	minOrMax $ (parMap rwhnf) (\gs -> minimax gs (not minimize) genSuccessors ev term) (genSuccessors gameState minimize)
	where minOrMax = if minimize then minimum else maximum -}

maxFstPair	:: (Ord a) => (a,b) -> (a,b) -> (a,b)
maxFstPair x@(a1,b1) (a2,b2) | a1 >= a2 = x
maxFstPair _ y = y

minFstPair	:: (Ord a) => (a,b) -> (a,b) -> (a,b)
minFstPair x@(a1,b1) (a2,b2) | a1 <= a2 = x
minFstPair _ y = y

opFstPairList	:: (Ord a) => ((a,b) -> (a,b) -> (a,b)) -> [(a,b)] -> (a,b)
opFstPairList op [] = error "Don't call this with empty list"
opFstPairList op ((a,b):[]) = (a,b)
opFstPairList op ((a,b):xs) = op (a,b) (opFstPairList op xs) 

minimax	:: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit | depth == depthlimit || terminalState gs = (evaluateState gs, Nothing)
minimax gs minimize depth depthlimit =
	let	minOrMax = opFstPairList (if minimize then minFstPair else maxFstPair)
		successors = (genSuccessors gs)
		scores = map fst $ map (\succ -> (minimax (makeSuccessor gs succ) (not minimize) (depth+1) depthlimit)) successors
		wrappedSuccessors = map Just successors
		scoreSuccPairs = zip scores wrappedSuccessors in
	minOrMax scoreSuccPairs

alphabetafold	:: (GameState a) => a -> [Int] -> Int -> Int -> Int -> Int -> Int
alphabetafold _ [] alpha _ _ _ = alpha
alphabetafold gs (x:xs) alpha beta depth depthlimit = 
	let	child = makeSuccessor gs x
		newAlpha = negate $ alphabeta child (depth+1) depthlimit (-beta) (-alpha) in
	if (beta <= newAlpha)
	then alpha
	else alphabetafold gs xs (max alpha newAlpha) beta depth depthlimit

alphabeta	:: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabeta gs _ _ _ _ | terminalState gs = trace "Reached terminal state." (evaluateState gs)
alphabeta gs depth depthlimit _ _ | depth == depthlimit = evaluateState gs
alphabeta gs depth depthlimit alpha beta =
	alphabetafold gs successors alpha beta depth depthlimit
	where 	successors = genSuccessors gs
{-		alphabetafold [] a _ = a
		alphabetafold (x:xs) a b = 
			let	child = makeSuccessor gs x
				newAlpha = negate $ alphabeta child (depth+1) depthlimit (-b) (-a) in
			if (b <= newAlpha)
			then a
			else alphabetafold xs (max a newAlpha) b-}
