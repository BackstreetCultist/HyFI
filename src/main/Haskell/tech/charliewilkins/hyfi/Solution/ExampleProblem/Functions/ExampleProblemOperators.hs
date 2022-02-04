module ExampleProblemOperators where

import RandomOperators (getSeed, getRandomIndex)
import ExampleProblemTypes

-- LOCAL SEARCH ---------------------------------------------------------------
randomBitFlip :: Solution -> OperatorMagnitude -> Solution
randomBitFlip s 0 = s
randomBitFlip s m = randomBitFlip (take i s ++ [bit] ++ drop (i+1) s) (m-1)
                where
                    bit = if (s !! i) == '0' then '1' else '0'
                    i = getRandomIndex (getSeed s) s
