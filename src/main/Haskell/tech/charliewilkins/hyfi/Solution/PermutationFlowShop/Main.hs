module Solution.PermutationFlowShop.Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.Functions.Helpers.RandomOperators (randomiseList)
import Solution.PermutationFlowShop.Types
import Solution.PermutationFlowShop.Functions.Acceptors (naiveAcceptor, improvingAcceptor)
import Solution.PermutationFlowShop.Functions.Evaluators (newObjectiveValue, improvement)
import Solution.PermutationFlowShop.Functions.Operators (randomSwap)

generator :: Int -> Instance -> Solution
generator s i = randomiseList s (take (length (head i)) [0..])

getOperatorsByClass :: [[Operator]]
getOperatorsByClass = [[randomSwap]]

getAcceptors :: [Acceptor]
getAcceptors = [naiveAcceptor, improvingAcceptor]

getEvaluators :: [Evaluator]
getEvaluators = [improvement]

getProblemInstance :: String -> Instance
getProblemInstance file = unsafePerformIO (loadProblemInstance file)

loadProblemInstance :: String -> IO (Instance)
loadProblemInstance file = do
                    let filePath =  "Solution/PermutationFlowShop/Instances/" ++ file
                    print filePath
                    handle <- openFile filePath ReadMode
                    contents <- hGetContents handle
                    putStr contents
                    print (lines contents)
                    print (head (lines contents))
                    let machines = [map read (words x) | x <- lines contents]
                    hClose handle
                    return (tail (reverse machines))

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
