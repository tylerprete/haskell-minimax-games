module Minimax
where
import Debug.Trace
import Data.List
import Data.Ord

class GameState a where
    evaluateState    :: a -> Int
    terminalState    :: a -> Bool
    genSuccessors    :: a -> [Int]
    makeSuccessor    :: a -> Int -> a
    isMaximizing    :: a -> Bool

minimax    :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit | depth == depthlimit || terminalState gs = (evaluateState gs, Nothing)
minimax gs minimize depth depthlimit =
    let minOrMax = (if minimize then minimumBy else maximumBy) (comparing fst)
        successors = (genSuccessors gs)
        scores = map fst $ map (\succ -> (minimax (makeSuccessor gs succ) (not minimize) (depth+1) depthlimit)) successors
        wrappedSuccessors = map Just successors
        scoreSuccPairs = zip scores wrappedSuccessors in
    minOrMax scoreSuccPairs

{- alphabetafold    :: (GameState a) => a -> [Int] -> Int -> Int -> Int -> Int -> Int
alphabetafold _ [] alpha _ _ _ = alpha
alphabetafold gs (x:xs) alpha beta depth depthlimit =
    let    child = makeSuccessor gs x
        newAlpha = negate $ alphabeta child (depth+1) depthlimit (-beta) (-alpha) in
    if (beta <= newAlpha)
    then alpha
    else alphabetafold gs xs (max alpha newAlpha) beta depth depthlimit -}

alphabeta   :: (GameState a) => a -> Int -> Int -> Int -> Int -> (Int, Maybe Int)
alphabeta gs _ _ _ _ | terminalState gs = (evaluateState gs, Nothing)
alphabeta gs depth depthlimit _ _ | depth == depthlimit = (evaluateState gs, Nothing)
alphabeta gs depth depthlimit alpha beta =
    alphabetafold successors alpha beta (-1)
    where   successors = genSuccessors gs
            alphabetafold [] a _ bestChild = (a, Just bestChild)
            alphabetafold (x:xs) a b bestChild =
                let child = makeSuccessor gs x
                    newAlpha = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
                if (newAlpha >= b)
                then (newAlpha, Just x)
                else alphabetafold xs (max a newAlpha) b (if newAlpha > a then x else bestChild)
{-alphabeta    :: (GameState a) => a -> Int -> Int -> Int -> Int -> (Int, Maybe Int)
alphabeta gs _ _ _ _ | terminalState gs = (evaluateState gs, Nothing)
alphabeta gs depth depthlimit _ _ | depth == depthlimit = (evaluateState gs, Nothing)
alphabeta gs depth depthlimit alpha beta =
    alphabetafold successors alpha beta (-1)
    where     successors = genSuccessors gs
        alphabetafold [] a _ bestChild = (a, Just bestChild)
        alphabetafold (x:xs) a b bestChild =
            let    child = makeSuccessor gs x
                (newAlph, _) = alphabeta child (depth+1) depthlimit (negate b) (negate a)
                newAlpha = negate newAlph in
            if (newAlpha >= b)
            then (newAlpha, Just x)
            else alphabetafold xs (max a newAlpha) b (if newAlpha > a then x else bestChild)
-}

alphabetamax    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamax gs _ _ _ _ | terminalState gs = evaluateState gs
alphabetamax gs depth depthlimit _ _ | depth == depthlimit = evaluateState gs
alphabetamax gs depth depthlimit alpha beta =
    alphabetafold successors alpha beta
    where   successors = genSuccessors gs
            alphabetafold [] a _ = a
            alphabetafold (x:xs) a b =
                let child = makeSuccessor gs x
                    newAlpha = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
                if (newAlpha >= b)
                then newAlpha
                else alphabetafold xs (max a newAlpha) b

alphabetamin    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamin gs _ _ _ _ | terminalState gs = evaluateState gs
alphabetamin gs depth depthlimit _ _ | depth == depthlimit = evaluateState gs
alphabetamin gs depth depthlimit alpha beta =
    alphabetafold successors alpha beta
    where   successors = genSuccessors gs
            alphabetafold [] _ b = b
            alphabetafold (x:xs) a b =
                let child = makeSuccessor gs x
                    newBeta = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
                if (newBeta <= a)
                then newBeta
                else alphabetafold xs a (min b newBeta)
