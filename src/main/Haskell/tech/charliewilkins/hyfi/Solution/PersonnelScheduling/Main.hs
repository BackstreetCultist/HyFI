module Solution.PersonnelScheduling.Main where

import Solution.PersonnelScheduling.Types
import Solution.PersonnelScheduling.Functions.Acceptors
import Solution.PersonnelScheduling.Functions.Evaluators
import Solution.PersonnelScheduling.Functions.Operators

generator :: Int -> Instance -> Solution

getOperatorsByClass :: [[Operator]]

getAcceptors :: [Acceptor]

getEvaluators :: [Evaluator]

getProblemInstance :: String -> Instance

getObjectiveValue :: Evaluator
