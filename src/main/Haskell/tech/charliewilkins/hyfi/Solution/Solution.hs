module Solution.Solution where

import Data.Typeable
import Data.Char (digitToInt)
import Data.List (foldl')

import HyperHeuristic.Types.HyperHeuristicTypes

import Solution.BinPacking.BinPacking (generator, getProblemInstance, getOperatorsByClass, getEvaluators, getAcceptors, getObjectiveValue)
import Solution.BinPacking.Types.BinPackingTypes

-- Application needs this on other side
type SolutionPopulation = (Instance, [Solution])

-- STARTUP --------------------------------------------------------------------

getInstance :: String -> Instance
getInstance = getProblemInstance

generateSolution :: Int -> Instance -> Solution
generateSolution = generator

generateSolutionList :: Int -> Int -> Instance -> [Solution]
generateSolutionList 0 _ i = []
generateSolutionList n seed i = generateSolution seed i : generateSolutionList (n-1) (seed+1) i

generateSolutionPopulationOfSize :: Int -> Int -> Instance -> SolutionPopulation
generateSolutionPopulationOfSize n seed i = (i, generateSolutionList n seed i)

-- BUILDING HEURISTIC ---------------------------------------------------------

-- Remember that anything mod n is 0 and n inclusive
-- So need to take 1 from length to get safe indexes
-- We must also add 1 to binaryVal to prevent divide by zero
-- And that magnitude should be at least 1
buildHeuristic :: HeuristicRepresentation -> BuiltHeuristic
buildHeuristic h = (selectOperator h, selectOperatorMagnitude h, selectAcceptor h, selectEvaluator h)

selectOperator :: HeuristicRepresentation -> Operator
selectOperator h = binaryToItem (substring 0 3 h) operators
                where
                    operators = binaryToItem (substring 3 6 h) getOperatorsByClass

selectOperatorMagnitude :: HeuristicRepresentation -> OperatorMagnitude
selectOperatorMagnitude h = (binaryVal (substring 6 9 h))+1

selectAcceptor :: HeuristicRepresentation -> Acceptor
selectAcceptor h = binaryToItem (substring 9 12 h) getAcceptors

selectEvaluator :: HeuristicRepresentation -> Evaluator
selectEvaluator h = binaryToItem (substring 12 15 h) getEvaluators

-- Get item from list with reference to binary data,
-- where there is no direct 1-to-1 relationship
binaryToItem :: [Char] -> [a] -> a
binaryToItem bits xs = xs !! (((length xs)-1) `mod` ((binaryVal bits)+1))
-- Implemented here using modulo

-- Substring between start (inclusive) and end (exclusive)
substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

binaryVal :: String -> Int
binaryVal xs = foldl' (\acc x -> acc * 2 + digitToInt x) 0 xs
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell

-- RUNNING HEURISTIC ----------------------------------------------------------

-- Returns a list of new solutions with their scores according to the heuristic
applyHeuristicRepresentationToPopulation :: HeuristicRepresentation -> SolutionPopulation -> [(Solution, Int)]
applyHeuristicRepresentationToPopulation h (i, ss) = map (runHeuristic (buildHeuristic h) i) ss

runHeuristic :: BuiltHeuristic -> Instance -> Solution -> (Solution, Int)
runHeuristic (op, opMag, acc, eval) i s = (s'', eval s s'' i)
                                    where
                                        s' = op s opMag i
                                        s'' = if (acc s s' i) then s' else s

-- EVALUATING SOLUTION --------------------------------------------------------
evaluateSolution :: Solution -> Instance -> Int
evaluateSolution s i = getObjectiveValue [] s i
