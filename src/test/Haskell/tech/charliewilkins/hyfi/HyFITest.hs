module HyFITest where

import Helpers.RandomOperatorsTest (checkRandomOperators)
import HyperHeuristic.MainTest (checkHyperHeuristic)

main = do
    checkHyperHeuristic
    checkRandomOperators
