module HyFITest where

import Helpers.RandomOperatorsTest (checkRandomOperators)
import HyperHeuristic.MainTest (checkHyperHeuristic)

checkHyFI = do
    checkHyperHeuristic
    checkRandomOperators
