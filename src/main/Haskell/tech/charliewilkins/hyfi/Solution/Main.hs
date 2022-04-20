module Solution.Main where

import Control.Monad.State
import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Typeable
import Data.Char (digitToInt)
import Data.List (foldl')

import HyperHeuristic.Types

import ProblemModules.BooleanSatisfiability.Main (generator, getProblemInstance, getOperatorsByClass, getEvaluators, getAcceptors, getObjectiveValue)
import ProblemModules.BooleanSatisfiability.Types

type SolutionPopulation = (Instance, [[Solution]])

-- STARTUP --------------------------------------------------------------------

getInstance :: String -> Instance
getInstance = getProblemInstance

generateSolution :: Int -> Instance -> Solution
generateSolution = generator

generateSolutionList :: Int -> Int -> Instance -> [Solution]
generateSolutionList 0 _ _ = []
generateSolutionList n seed i = generateSolution seed i : generateSolutionList (n-1) (seed+1) i

generateSolutionListOfLists :: Int -> Int -> Instance -> [[Solution]]
generateSolutionListOfLists 0 _ _ = []
generateSolutionListOfLists n seed i = generateSolutionList 1 seed i : generateSolutionListOfLists (n-1) (seed+1) i

generateSolutionPopulationOfSize :: Int -> Int -> Instance -> SolutionPopulation
generateSolutionPopulationOfSize n seed i = (i, generateSolutionListOfLists n seed i)

-- BUILDING HEURISTIC ---------------------------------------------------------

-- Remember that anything mod n is 0 and n inclusive
-- So need to take 1 from length to get safe indexes
-- We must also add 1 to binaryVal to prevent divide by zero
-- And that magnitude should be at least 1
buildHeuristic :: HeuristicRepresentation -> BuiltHeuristic
buildHeuristic h = (selectOperator h, selectOperatorMagnitude h, selectAcceptor h, selectEvaluator h)

selectOperator :: HeuristicRepresentation -> Operator
selectOperator h = binaryToItem (substring 0 3 h) operators
                where operators = binaryToItem (substring 3 6 h) getOperatorsByClass

selectEvaluator :: HeuristicRepresentation -> Evaluator
selectEvaluator h = binaryToItem (substring 6 9 h) getEvaluators

selectAcceptor :: HeuristicRepresentation -> Acceptor
selectAcceptor h = binaryToItem (substring 9 12 h) getAcceptors

selectOperatorMagnitude :: HeuristicRepresentation -> OperatorMagnitude
selectOperatorMagnitude h = (binaryVal (substring 12 16 h))+1

-- Get item from list with reference to binary data,
-- where there is no direct 1-to-1 relationship
binaryToItem :: [Char] -> [a] -> a
binaryToItem bits xs = xs !! (binaryVal bits `mod` length xs)

-- Substring between start (inclusive) and end (exclusive)
substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

binaryVal :: String -> Int
binaryVal xs = foldl' (\acc x -> acc * 2 + digitToInt x) 0 xs
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell

-- RUNNING HEURISTIC ----------------------------------------------------------

applyPopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
applyPopulation set = do
                    (i, sss) <- get
                    let hs = evalState set (i, sss)
                    let results = parMap rdeepseq (\x -> applyHeuristicRepresentationToSolutions (fst (fst x)) i (snd x)) (zip hs sss) -- [[(Solution, Int)]]
                    let hs' = map (\((h, (s,r)), result) -> (h, (s + sum (snd (unzip result)), r+1))) (zip hs results) -- HeuristicPopulation
                    let sss' = (map fst (map unzip results)) -- [[Solution, Int]] -> [[Solution]]
                    put (i, (sss' ++ (drop (length sss') sss))) -- Keep any excess solution sets that were not run
                    return hs'

-- Takes the set of solutions to apply the heuristic to
-- Returns a list of new solutions with their scores according to the heuristic
applyHeuristicRepresentationToSolutions :: HeuristicRepresentation -> Instance -> [Solution] -> [(Solution, Int)]
applyHeuristicRepresentationToSolutions h i ss = parMap rdeepseq (runHeuristic (buildHeuristic h) i) ss

runHeuristic :: BuiltHeuristic -> Instance -> Solution -> (Solution, Int)
runHeuristic (op, opMag, acc, eval) i s = (s'', if not accepted then -1 else eval s s'' i)
                                    where
                                        s' = op s opMag i
                                        accepted = (acc s s' i)
                                        s'' = if accepted then s' else s

-- EVALUATING SOLUTION --------------------------------------------------------
evaluateSolution :: Solution -> Instance -> Int
evaluateSolution s i = getObjectiveValue [] s i
