module Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityEvaluators where

import Solution.BooleanSatisfiability.Types.BooleanSatisfiabilityTypes

newObjectiveValue :: Evaluator
newObjectiveValue old new (_, clauses) = sum (map (evaluateClause new) clauses)

-- Returns 1 if the clause evaluates to true, and 0 if false
evaluateClause :: Solution -> [Int] -> Int
evaluateClause sol clause = if sum (map (checkVar sol) clause) == length clause then 1 else 0

-- Returns 1 if the variable is met, and 0 otherwise
-- The var may be negative, in which case we expect it to be False
checkVar :: Solution -> Int -> Int
checkVar sol var | (var > 0) = if (sol !! ((abs var)-1) == '1') then 1 else 0
                 | otherwise = if (sol !! ((abs var)-1) == '1') then 0 else 1

-- Second clause evaluates old solution
improvement :: Evaluator
improvement old new i = (newObjectiveValue old new i ) - (newObjectiveValue new old i)

newPercentageCorrect :: Evaluator
newPercentageCorrect _ s i = floor (((fromIntegral correct :: Double) / (fromIntegral (length (snd i) ) :: Double)) * 100)
                        where
                            correct = newObjectiveValue [] s i
