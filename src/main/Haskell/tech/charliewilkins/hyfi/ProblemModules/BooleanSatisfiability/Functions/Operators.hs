module ProblemModules.BooleanSatisfiability.Functions.Operators where

import Helpers.RandomOperators (getSeed, getRandomIndex)
import ProblemModules.BooleanSatisfiability.Functions.Evaluators (evaluateClause)
import ProblemModules.BooleanSatisfiability.Types

-- LOCAL SEARCH HEURISTICS ----------------------------------------------------
flipRandomVariable :: Operator
flipRandomVariable s 0 _ = s
flipRandomVariable s m inst = flipRandomVariable (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                where
                                    bit = if (s !! i) == '0' then '1' else '0'
                                    i = getRandomIndex (getSeed s) s
--TODO having no seed besides the time means that you only change the same var each time

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

reverseSolution :: Operator
reverseSolution s _ _ = reverse s

invertSolution :: Operator
invertSolution s _ _ = [if x == '0' then '1' else '0' | x <- s]
