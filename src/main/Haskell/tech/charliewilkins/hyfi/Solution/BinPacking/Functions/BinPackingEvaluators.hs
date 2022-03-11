module Solution.BinPacking.Functions.BinPackingEvaluators where

import Solution.BinPacking.Types.BinPackingTypes

newObjectiveValue :: Evaluator
newObjectiveValue _ new _ = length new

-- Second clause evaluates old solution
improvement :: Evaluator
improvement old new i = (newObjectiveValue old new i ) - (newObjectiveValue new old i)
