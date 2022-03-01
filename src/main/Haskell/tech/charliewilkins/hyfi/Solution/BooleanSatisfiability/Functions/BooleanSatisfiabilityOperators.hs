module BooleanSatisfiabilityOperators where

import RandomOperators (getSeed', getRandomIndex)
import BooleanSatisfiabilityTypes

-- Local Search Heuristics
flipRandomVariable :: Operator
flipRandomVariable s 0 _ = s
flipRandomVariable s m inst = flipRandomVariable (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                where
                                    bit = if (s !! i) == True then False else True
                                    i = getRandomIndex getSeed' s
--TODO having no seed besides the time means that you only change the same var each time

flipRandomVariableFromBrokenClause :: Operator
flipRandomVariableFromBrokenClause s 0 _ = s
flipRandomVariableFromBrokenClause s m inst = flipRandomVariableFromBrokenClause (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                        where
                                            bit = if (s !! i) == True then False else True
                                            i = getRandomIndex getSeed' s
