module Solution.PermutationFlowShop.Functions.Evaluators where

import Control.Parallel (par, pseq)

import Solution.PermutationFlowShop.Types

newObjectiveValue :: Evaluator
newObjectiveValue _ new i = endTime ((length new)-1) new ((length i)-1) i

endTime :: Int -> Solution -> Int -> Instance -> Int
endTime _ _ 0 _ = 0
endTime 0 _ _ _ = 0
endTime j js m ms = (lhs `par` rhs `pseq` (max lhs rhs)) + ((ms !! m) !! (js !! j))
                    where
                        lhs = endTime j js (m-1) ms
                        rhs = endTime (j-1) js m ms
-- https://www.researchgate.net/publication/222817911_The_distributed_permutation_flowshop_scheduling_problem

-- Second clause evaluates old solution
-- Need to make it positive
improvement :: Evaluator
improvement old new i = 0 - ((newObjectiveValue old new i ) - (newObjectiveValue new old i))
