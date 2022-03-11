module Solution.BinPacking.Functions.BinPackingEvaluators where

import Solution.BinPacking.Types.BinPackingTypes

newObjectiveValue :: Evaluator
newObjectiveValue _ new _ = length new
