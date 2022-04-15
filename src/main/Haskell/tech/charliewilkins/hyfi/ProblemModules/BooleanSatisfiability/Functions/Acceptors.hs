module ProblemModules.BooleanSatisfiability.Functions.Acceptors where

import System.Random (randomR, mkStdGen)

import Helpers.RandomOperators (getSeed)
import ProblemModules.BooleanSatisfiability.Types
import ProblemModules.BooleanSatisfiability.Functions.Evaluators (improvement, newObjectiveValue, newPercentageCorrect)

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0

-- Accepts any improving move, or other moves on a 50/50 chance
improvingOrChanceAcceptor :: Acceptor
-- improvingOrChanceAcceptor old new i = if (improvement old new i) > 0 then True else fst (randomR (True, False) (mkStdGen (getSeed new)))
improvingOrChanceAcceptor old new i = if (improvement old new i) > 0 then True else (fst (randomR ('a', 'd') (mkStdGen (getSeed new))) == 'a')

-- Accepts if the improvement is greater than or equal to 5% (where a perfect solution is 100%)
substantialImprovementAcceptor :: Acceptor
substantialImprovementAcceptor old new i = (newPercentageCorrect [] new i) - (newPercentageCorrect [] old i) >= 5
