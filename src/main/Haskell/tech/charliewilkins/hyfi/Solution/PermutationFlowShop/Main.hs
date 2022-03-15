module Solution.PermutationFlowShop.Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.Functions.Helpers.RandomOperators (randomiseList)
import Solution.PermutationFlowShop.Types
import Solution.PermutationFlowShop.Functions.Acceptors
import Solution.PermutationFlowShop.Functions.Evaluators
import Solution.PermutationFlowShop.Functions.Operators

generator :: Int -> Instance -> Solution
generator s i = randomiseList s (take (length (head i)) [0..])

getOperatorsByClass :: [[Operator]]

getAcceptors :: [Acceptor]

getEvaluators :: [Evaluator]

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
                    return machines

getObjectiveValue :: Evaluator
