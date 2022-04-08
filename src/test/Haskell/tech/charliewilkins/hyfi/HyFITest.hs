module HyFITest where

import Helpers.RandomOperatorsTest (checkRandomOperators)
import HyperHeuristic.MainTest (checkHyperHeuristic)
import Solution.MainTest (checkSolution)
import ProblemModules.BinPacking.MainTest (checkBinPacking)
import ProblemModules.BooleanSatisfiability.MainTest (checkBooleanSatisfiability)
import ProblemModules.Example.MainTest (checkExample)

main = do
    print "### Checking Random Operators ###"
    checkRandomOperators
    print "### Checking Hyper-Heuristic Layer ###"
    checkHyperHeuristic
    print "### Checking Problem Layer ###"
    checkSolution
    print "### Checking Problem Modules: Bin Packing ###"
    checkBinPacking
    print "### Checking Problem Modules: MAX-SAT ###"
    checkBooleanSatisfiability
    print "### Checking Problem Modules: Example ###"
    checkExample
