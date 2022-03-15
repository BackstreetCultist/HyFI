module Solution.PermutationFlowShop.Main where

import Solution.PermutationFlowShop.Types
import Solution.PermutationFlowShop.Functions.Acceptors
import Solution.PermutationFlowShop.Functions.Evaluators
import Solution.PermutationFlowShop.Functions.Operators

generator :: Int -> Instance -> Solution

getOperatorsByClass :: [[Operator]]

getAcceptors :: [Acceptor]

getEvaluators :: [Evaluator]

getProblemInstance :: String -> Instance

getObjectiveValue :: Evaluator
