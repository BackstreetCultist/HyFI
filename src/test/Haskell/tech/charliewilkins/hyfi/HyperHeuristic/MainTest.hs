module HyperHeuristic.MainTest where

import Data.List

import Test.QuickCheck

import HyperHeuristic.Main
import Helpers.RandomOperators (getRandomIndex)

-- STARTUP --------------------------------------------------------------------
checkStartup = do
    checkGenerateHeuristic
    checkGenerateHeuristicPopulationOfSize

-- generateHeuristic
-- GIVEN a random seed
-- WHEN I call generateHeuristic
checkGenerateHeuristic = do
    checkHeuristicLength
    checkHeuristicChars

-- THEN I recieve a heuristic string of length 16
prop_generateHeuristic_len x = length (generateHeuristic x) == 16
checkHeuristicLength = quickCheck prop_generateHeuristic_len

-- AND the heuristic only contains binary chars
prop_generateHeuristic_chars x = (heuristic !! i == '0') || (heuristic !! i == '1')
    where
        heuristic = generateHeuristic x
        i = getRandomIndex (x+1) heuristic
checkHeuristicChars = quickCheck prop_generateHeuristic_chars

--generateHeuristicPopulationOfSize
-- GIVEN a positive size n
-- AND a random seed
-- WHEN I call generateHeuristicPopulationOfSize
checkGenerateHeuristicPopulationOfSize = do
    checkHeuristicPopulationSize

-- THEN I recieve a population of size n
prop_generateHeuristicPopulationOfSize_len (Positive n) x = length (generateHeuristicPopulationOfSize n x) == n
checkHeuristicPopulationSize = quickCheck prop_generateHeuristicPopulationOfSize_len

-- EVOLUTION ------------------------------------------------------------------
checkEvolution = do
    checkSurvival
    checkReproduction
    checkMutation

checkSurvival = do
    return ()

checkReproduction = do
    checkReproductionStep
    checkGenerateChildren

-- reproductionStep
-- GIVEN a heuristic population
-- WHEN I call reproductionStep
checkReproductionStep = do
    checkReproductionStepLength
-- THEN I recieve a population of length 2
prop_checkReproduction_len hPop = length (reproductionStep hPop) == 2
checkReproductionStepLength = quickCheck prop_checkReproduction_len

-- generateChildren
-- GIVEN a pair of heuristics
-- WHEN I call generateChildren
checkGenerateChildren = do
    checkGenerateChildrenLength
    checkGenerateChildrenChildLength
-- THEN I recieve a population of length 2
prop_checkGenerateChildren_len (x,y) = length (generateChildren ((generateHeuristic x),(generateHeuristic y))) == 2
checkGenerateChildrenLength = quickCheck prop_checkGenerateChildren_len
-- AND each child is the correct length
prop_generateChildren_childLen (x,y) = length (fst (head (generateChildren ((generateHeuristic x),(generateHeuristic y))))) == length (generateHeuristic x) && length (fst (head (reverse (generateChildren ((generateHeuristic x),(generateHeuristic y)))))) == length (generateHeuristic x)
checkGenerateChildrenChildLength = quickCheck prop_generateChildren_childLen

checkMutation = do
    return ()

-- GENERAL --------------------------------------------------------------------
checkHyperHeuristic = do
    checkStartup
    checkEvolution
