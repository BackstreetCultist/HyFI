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

newPercentageCorrect :: Evaluator
newPercentageCorrect _ s i = floor (((fromIntegral correct :: Double) / (fromIntegral (length (snd i) ) :: Double)) * 100)
                        where
                            correct = newObjectiveValue [] s i

-- Second clause evaluates old solution
improvement :: Evaluator
improvement old new i = (newObjectiveValue old new i ) - (newObjectiveValue new old i)

percentageImprovement :: Evaluator
percentageImprovement old new i = (newPercentageCorrect old new i) - (newPercentageCorrect new old i)

searchSpaceDistance :: Evaluator
searchSpaceDistance [] [] _ = 0
searchSpaceDistance (old:olds) (new:news) i = (if old==new then 0 else 1) + searchSpaceDistance olds news i

-- A var's magnitude is taken as the number of clauses it appears in
-- Thus magnitude distance is the sum of magnitudes of vars changed
magnitudeDistance :: Evaluator
magnitudeDistance old new i = sum (map (\((x,y)) -> if x==(snd y) then 0 else magnitude (fst y) i) (zip old (zip [1..] new)))

magnitude :: Int -> Instance -> Int
magnitude var (c, []) = 0
magnitude var (c, (clause:clauses)) = (if (elem var clause) || (elem (0-var) clause) then 1 else 0) + magnitude var (c, clauses) 
