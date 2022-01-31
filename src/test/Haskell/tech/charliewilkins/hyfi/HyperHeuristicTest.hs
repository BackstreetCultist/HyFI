module HyperHeuristicTest where

import Test.QuickCheck

import HyperHeuristic

-- STARTUP --------------------------------------------------------------------
-- generateHeuristic
-- GIVEN a random seed
-- WHEN I call generateHeuristic
-- THEN I recieve a heuristic string of length 16
-- AND the heuristic only contains binary chars
prop_generateHeuristic_len x = length (generateHeuristic x) == 16
checkHeuristicLength = quickCheck prop_generateHeuristic_len

prop_generateHeuristic_chars x = (heuristic !! i == '0') || (heuristic !! i == '1')
    where
        heuristic = generateHeuristic x
        i = getRandomIndex (x+1) heuristic
checkHeuristicChars = quickCheck prop_generateHeuristic_chars

checkGenerateHeuristic = do
    checkHeuristicLength
    checkHeuristicChars

--generateHeuristicPopulationOfSize
-- GIVEN a positive size n
-- AND a random seed
-- WHEN I call generateHeuristicPopulationOfSize
-- THEN I recieve a population of size n

prop_generateHeuristicPopulationOfSize_len (Positive n) x = length (generateHeuristicPopulationOfSize n x) == n
checkHeuristicPopulationSize = quickCheck prop_generateHeuristicPopulationOfSize_len

checkGenerateHeuristicPopulationOfSize = do
    checkHeuristicPopulationSize

checkStartup = do
    checkGenerateHeuristic
    checkGenerateHeuristicPopulationOfSize
