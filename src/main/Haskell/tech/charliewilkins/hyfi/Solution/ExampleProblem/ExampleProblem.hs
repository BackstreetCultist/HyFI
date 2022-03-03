module ExampleProblem where

import System.Random (randomRs, mkStdGen)

import ExampleProblemTypes
import ExampleProblemOperators (randomBitFlip)
import ExampleProblemAcceptors (naiveAcceptor, improvingAcceptor)
import ExampleProblemEvaluators (newObjectiveValue, improvement, valueDistance, searchSpaceDistance)

generator :: Int -> Solution
generator seed = take 8 (randomRs ('0', '1') (mkStdGen seed))

getOperatorsByClass :: [[Operator]]
getOperatorsByClass = [[randomBitFlip]]

getAcceptors :: [Acceptor]
getAcceptors = [naiveAcceptor, improvingAcceptor]

getEvaluators :: [Evaluator]
getEvaluators = [newObjectiveValue, improvement, valueDistance, searchSpaceDistance]

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
