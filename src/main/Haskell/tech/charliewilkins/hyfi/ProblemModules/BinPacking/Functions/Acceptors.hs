module ProblemModules.BinPacking.Functions.Acceptors where

import System.Random (randomR, mkStdGen)

import Helpers.RandomOperators (getSeed')
import ProblemModules.BinPacking.Functions.Evaluators (improvement, averageFullnessIncrease, fullBinsImprovement)
import ProblemModules.BinPacking.Types

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0

-- Accepts any improving move, or other moves on a 50/50 chance
improvingOrChanceAcceptor :: Acceptor
improvingOrChanceAcceptor old new i = if (improvement old new i) > 0 then True else fst (randomR (True, False) (mkStdGen (getSeed' + length (head new))))

averageFullnessImprovingAcceptor :: Acceptor
averageFullnessImprovingAcceptor old new i = (averageFullnessIncrease old new i) > 0

fullBinsImprovingAcceptor :: Acceptor
fullBinsImprovingAcceptor old new i = (fullBinsImprovement old new i) > 0
