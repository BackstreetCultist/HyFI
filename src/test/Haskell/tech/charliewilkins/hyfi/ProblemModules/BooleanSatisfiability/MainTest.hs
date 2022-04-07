{-# LANGUAGE ScopedTypeVariables #-}

module ProblemModules.BooleanSatisfiability.MainTest where

import Data.List
import Test.QuickCheck

import Helpers.RandomOperators (randomiseList)
import ProblemModules.BooleanSatisfiability.Main
import ProblemModules.BooleanSatisfiability.Types
import ProblemModules.BooleanSatisfiability.Functions.Operators
import ProblemModules.BooleanSatisfiability.Functions.Acceptors
import ProblemModules.BooleanSatisfiability.Functions.Evaluators

-- STARTUP --------------------------------------------------------------------
checkStartup :: IO ()
checkStartup = do
    checkGenerator

-- generator
-- GIVEN a problem instance
-- WHEN I call to generate a solution
checkGenerator = do
    checkGeneratorLength
-- AND the instance has a positive number of variables
-- THEN I receive a solution of the same length as the number of variables
prop_checkGenerator_len s i = fst i > 0 ==> length (generator s i) == (fst i)
checkGeneratorLength = quickCheck prop_checkGenerator_len

-- OPERATORS ------------------------------------------------------------------
checkOperators = do
    checkFlipRandomVariable
    checkReduceClauses
    checkGsat
    checkGetRandomIndexOfHighestGainVariables
    checkRandomlyReinitialise

-- flipRandomVariable
-- GIVEN a solution
-- WHEN I call the flipRandomVariable operator
checkFlipRandomVariable = do
    checkFlipRandomVariableChange
    checkFlipRandomVariableMultiChange
-- AND I set a magnitude of 1
-- THEN there is one difference between the two solutions
prop_checkFlipRandomVariable_chg s i = s > 0 ==> sum [if x == y then 0 else 1 | (x,y) <- zip (take s (repeat '0')) (flipRandomVariable (take s (repeat '0')) 1 i)] == 1
checkFlipRandomVariableChange = quickCheck prop_checkFlipRandomVariable_chg
-- AND I set the magnitude to an odd number
-- THEN there is at least one difference between the two solutions
prop_checkFlipRandomVariable_mchg s i = (s > 0) && (s `mod` 2 == 1) ==> sum [if x == y then 0 else 1 | (x,y) <- zip (take s (repeat '0')) (flipRandomVariable (take s (repeat '0')) 1 i)] >= 1
checkFlipRandomVariableMultiChange = quickCheck prop_checkFlipRandomVariable_mchg

-- reduceClauses
-- GIVEN a set of clauses
-- WHEN I request the clauses in which a given variable appears
checkReduceClauses = do
    checkReduceClausesPositive
    checkReduceClausesNegative
-- THEN the variable appears in all the clauses
prop_checkReduceClauses_pos x ys = all (== True) (map (elem x) (reduceClauses x ys))
checkReduceClausesPositive = quickCheck prop_checkReduceClauses_pos
-- AND there are no clauses missed in which the variable appears
prop_checkReduceClauses_neg x ys = all (== False) (map (elem x) [y | y <- ys, not (elem y (reduceClauses x ys))])
checkReduceClausesNegative = quickCheck prop_checkReduceClauses_neg

-- gsat
-- GIVEN a solution
-- WHEN I call the gsat operator
checkGsat = do
    checkGsatChange
    checkGsatMultiChange
-- AND I set a magnitude of 1
-- THEN there is one difference between the two solutions
prop_checkGsat_chg s i = s > 0 ==> sum [if x == y then 0 else 1 | (x,y) <- zip (take s (repeat '0')) (gsat (take s (repeat '0')) 1 (s, (take i (repeat [1]))))] == 1
checkGsatChange = quickCheck prop_checkGsat_chg
-- AND I set the magnitude to an odd number
-- THEN there is at least one difference between the two solutions
prop_checkGsat_mchg s i = (s > 0) && (s `mod` 2 == 1) ==> sum [if x == y then 0 else 1 | (x,y) <- zip (take s (repeat '0')) (gsat (take s (repeat '0')) 1 (s, (take i (repeat [1]))))] >= 1
checkGsatMultiChange = quickCheck prop_checkGsat_mchg

-- getRandomIndexOfHighestGainVariables
-- GIVEN a solution and instance
-- WHEN I request the index of a high-gain variable
checkGetRandomIndexOfHighestGainVariables = do
    checkGetRandomIndexOfHighestGainVariablesSort
    checkGetRandomIndexOfHighestGainVariablesValue
-- AND I receive an index whose gain is greater than or equal to that of the one behind it in the list
prop_checkGetRandomIndexOfHighestGainVariables_srt seed = fst (head (reverse (sort (randomiseList seed (getClausesNextToImprovement s i))))) >= fst (head (tail (reverse (sort (randomiseList seed (getClausesNextToImprovement s i))))))
                                                        where
                                                            s = "00000"
                                                            i = (5, [[-1, -2], [3], [3,4], [5]])
checkGetRandomIndexOfHighestGainVariablesSort = quickCheck prop_checkGetRandomIndexOfHighestGainVariables_srt
-- AND the gain is calculated correctly
prop_checkGetRandomIndexOfHighestGainVariables_val seed = fst (head (reverse (sort (randomiseList seed (getClausesNextToImprovement s i))))) == 2
                                                        where
                                                            s = "00000"
                                                            i = (5, [[-1, -2], [3], [3,4], [5], [5]])
checkGetRandomIndexOfHighestGainVariablesValue = quickCheck prop_checkGetRandomIndexOfHighestGainVariables_val

-- randomlyReinitialise
-- GIVEN a solution and instance
-- WHEN I randomly reinitialise part of the solution
checkRandomlyReinitialise = do
    checkRandomlyReinitialiseLength
-- THEN I receive a new solution of the same length as the old one
prop_checkRandomlyReinitialise_len s m xs = s > 0 ==> s == length (randomlyReinitialise (take s (repeat '0')) m (s, xs))
checkRandomlyReinitialiseLength = quickCheck prop_checkRandomlyReinitialise_len

-- ACCEPTORS ------------------------------------------------------------------
checkAcceptors = do
    return ()

-- EVALUATORS -----------------------------------------------------------------
checkEvaluators = do
    checkEvaluateClause

-- evaluateClause
-- GIVEN a solution and a clause
-- WHEN I evaluate the clause
checkEvaluateClause = do
    checkEvaluateClausePositive
    checkEvaluateClauseNegative
-- AND the clause contains all positives
-- THEN I receive a 1 if the clause is satisfied
prop_checkEvaluateClause_pos (Positive s) (Positive c) = evaluateClause (take (s * 1000) (repeat '1')) (take c [1..]) == 1
checkEvaluateClausePositive = quickCheck prop_checkEvaluateClause_pos
-- AND the clause contains all positives
-- THEN I receive a 0 if the clause is unsatisfied
prop_checkEvaluateClause_neg (Positive s) (Positive c) = evaluateClause (take (s * 1000) (repeat '0')) (take c [1..]) == 0
checkEvaluateClauseNegative = quickCheck prop_checkEvaluateClause_neg

-- GENERAL --------------------------------------------------------------------
checkBooleanSatisfiability = do
    checkStartup
    checkOperators
    checkAcceptors
    checkEvaluators
