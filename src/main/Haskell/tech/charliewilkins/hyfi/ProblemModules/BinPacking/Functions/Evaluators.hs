module ProblemModules.BinPacking.Functions.Evaluators where

import Data.List

import ProblemModules.BinPacking.Types

newObjectiveValue :: Evaluator
newObjectiveValue _ new _ = length new

numberOfFullBins :: Evaluator
numberOfFullBins _ new i = sum [if (sum (map (\y -> (snd i) !! y) x)) == (fst i) then 1 else 0 | x <- new]

averageFullness :: Evaluator
averageFullness _ new i = (floor (sum [sum (map (\y -> (snd i) !! y) x) | x <- new])) `div` (length new)

hyFlexFitnessFunction :: Solution -> Solution -> Instance -> Double
hyFlexFitnessFunction _ new (cap, objs) = 1.0 - ((sum (map (\bin -> ((sum (map (\obj -> objs !! obj) bin) / cap) ^ 2)) new)) / (fromIntegral (length new) :: Double))

-- Second clause evaluates old solution
-- Need to flip it as Heuristics want high scores
    -- TODO can this be fixed in another way? Should it be?
improvement :: Evaluator
improvement old new i = 0 - ((newObjectiveValue old new i ) - (newObjectiveValue new old i))

fullBinsImprovement :: Evaluator
fullBinsImprovement old new i = (numberOfFullBins old new i) - (numberOfFullBins new old i)

searchSpaceDistance :: Evaluator
searchSpaceDistance old new _ = length (concat (old \\ new))

averageFullnessIncrease :: Evaluator
averageFullnessIncrease old new i = (averageFullness old new i) - (averageFullness new old i)
