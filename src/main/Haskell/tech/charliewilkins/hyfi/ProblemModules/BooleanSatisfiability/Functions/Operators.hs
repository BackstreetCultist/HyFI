module ProblemModules.BooleanSatisfiability.Functions.Operators where

import Data.List (sort)
import System.Random (randomRs, mkStdGen)

import Helpers.RandomOperators (getSeed, getRandomIndex, randomiseList)
import ProblemModules.BooleanSatisfiability.Functions.Evaluators (evaluateClause)
import ProblemModules.BooleanSatisfiability.Types

-- LOCAL SEARCH HEURISTICS ----------------------------------------------------
flipRandomVariable :: Operator
flipRandomVariable s 0 _ = s
flipRandomVariable s m inst = flipRandomVariable (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                where
                                    bit = if (s !! i) == '0' then '1' else '0'
                                    i = getRandomIndex (getSeed s) s

-- Same as above but the variable (i+1) must be in a clause that is broken
flipRandomVariableFromBrokenClause :: Operator
flipRandomVariableFromBrokenClause s 0 _ = s
flipRandomVariableFromBrokenClause s m inst = flipRandomVariableFromBrokenClause (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                            where
                                                bit = if (s !! i) == '0' then '1' else '0'
                                                i = getRandomIndexFromBrokenClause (getSeed s) s inst

getRandomIndexFromBrokenClause :: Int -> Solution -> Instance -> Int
getRandomIndexFromBrokenClause seed s inst = if inBrokenClause (i+1) s inst then i else getRandomIndexFromBrokenClause (seed+1) s inst
                                            where
                                                i = getRandomIndex seed s

inBrokenClause :: Int -> Solution -> Instance -> Bool
inBrokenClause var s (_, clauses) = if length reducedClauses == sum (map (evaluateClause s) reducedClauses) then False else True
                                    where
                                        reducedClauses = reduceClauses var clauses

--Remove all entries in list that i does not appear in
reduceClauses :: Int -> [[Int]] -> [[Int]]
reduceClauses i xs = [x | x <- xs, elem i x]

-- MUTATION HEURISTICS
gsat :: Operator
gsat s 0 _ = s
gsat s m inst = gsat (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                            where
                                                bit = if (s !! i) == '0' then '1' else '0'
                                                i = getRandomIndexOfHighestGainVariables (getSeed s) s inst

-- Returns the index of a variable which, if flipped, would cause the greatest possible improvement
getRandomIndexOfHighestGainVariables :: Int -> Solution -> Instance -> Int
getRandomIndexOfHighestGainVariables seed s i = snd (head (reverse (sort (randomiseList seed (getClausesNextToImprovement s i)))))

-- Returns a list of the improvement that flipping each variable (i+1) would result in, paired with that variables index in the list (i)
getClausesNextToImprovement :: Solution -> Instance -> [(Int, Int)]
-- getClausesNextToImprovement s i =  [(newObjectiveValue [] (flipVarAtIndex x s) i, x) | x <- take (length s) [0..]]
getClausesNextToImprovement s (_, clauses) = [(sum (map (evaluateClause (take x s ++ [if (s !! x) == '0' then '1' else '0'] ++ drop (x+1) s)) (reduceClauses (x+1) clauses)) - sum (map (evaluateClause s) (reduceClauses (x+1) clauses)), x) | x <- take (length s) [0..]]

-- RUIN-RECREATE
randomlyReinitialise :: Operator
randomlyReinitialise s m inst | m < 4 = (take ((length s) `div` 4) newRandomSolution) ++ (drop ((length s) `div` 4) s)
                              | m < 8 = (take (((length s) `div` 4)*2) newRandomSolution) ++ (drop (((length s) `div` 4)*2) s)
                              | m < 12 = (take (((length s) `div` 4)*3) newRandomSolution) ++ (drop (((length s) `div` 4)*3) s)
                              | otherwise =  newRandomSolution
                            where
                                newRandomSolution = take (fst inst) (randomRs ('0', '1') (mkStdGen (getSeed s)))
