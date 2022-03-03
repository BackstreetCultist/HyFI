module BooleanSatisfiabilityOperators where

import RandomOperators (getSeed, getRandomIndex)
import BooleanSatisfiabilityTypes

-- LOCAL SEARCH HEURISTICS ----------------------------------------------------
flipRandomVariable :: Operator
flipRandomVariable s 0 _ = s
flipRandomVariable s m inst = flipRandomVariable (take i s ++ [bit] ++ drop (i+1) s) (m-1) inst
                                where
                                    bit = if (s !! i) == '0' then '1' else '0'
                                    i = getRandomIndex (getSeed s) s
--TODO having no seed besides the time means that you only change the same var each time
