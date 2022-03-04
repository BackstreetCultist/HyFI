module Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityAcceptors where

import System.Random (randomR, mkStdGen)

import HyperHeuristic.Functions.Helpers.RandomOperators (getSeed)
import Solution.BooleanSatisfiability.Types.BooleanSatisfiabilityTypes
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityEvaluators (improvement, newObjectiveValue, newPercentageCorrect)

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0

-- Accepts any improving move, or other moves on a 50/50 chance
improvingOrChanceAcceptor :: Acceptor
improvingOrChanceAcceptor old new i = if (improvement old new i) > 0 then True else fst (randomR (True, False) (mkStdGen (getSeed new)))

-- Accepts if the improvement is greater than 5% (where a perfect solution is 100%)
substantialImprovementAcceptor :: Acceptor
substantialImprovementAcceptor old new i = (newPercentageCorrect [] new i) - (newPercentageCorrect [] new i) > 5
