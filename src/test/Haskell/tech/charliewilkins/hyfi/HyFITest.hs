module HyFITest where

import Helpers.RandomOperatorsTest (checkRandomOperators)
import HyperHeuristic.MainTest (checkHyperHeuristic)
import Solution.MainTest (checkSolution)

main = do
    checkRandomOperators
    checkHyperHeuristic
    checkSolution
