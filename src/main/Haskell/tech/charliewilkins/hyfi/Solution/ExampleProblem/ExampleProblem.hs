module Solution.ExampleProblem.ExampleProblem where

import System.Random (randomRs, mkStdGen)

import Solution.ExampleProblem.Types.ExampleProblemTypes
import Solution.ExampleProblem.Functions.ExampleProblemOperators (randomBitFlip)
import Solution.ExampleProblem.Functions.ExampleProblemAcceptors (naiveAcceptor, improvingAcceptor)
import Solution.ExampleProblem.Functions.ExampleProblemEvaluators (newObjectiveValue, improvement, valueDistance, searchSpaceDistance)

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
