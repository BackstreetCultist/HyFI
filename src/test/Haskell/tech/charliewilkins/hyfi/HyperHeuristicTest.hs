module HyperHeuristicTest where

import Test.QuickCheck

import HyperHeuristic

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
