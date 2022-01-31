module HyperHeuristicTest where

import Data.List

import Test.QuickCheck

import HyperHeuristic

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

-- HELPERS --------------------------------------------------------------------
checkHelpers = do
    checkGetRandomIndex
    checkRandomiseList

--getRandomIndex
-- GIVEN a random seed
-- AND a non-empty list
-- WHEN I ask for a random index in the list
checkGetRandomIndex = do
    checkGetRandomIndexRange

-- THEN I recieve an int in the range of the list
prop_getRandomIndex_inRange seed (NonEmpty xs) = (getRandomIndex seed xs >= 0) && (getRandomIndex seed xs < length xs)
checkGetRandomIndexRange = quickCheck (prop_getRandomIndex_inRange :: Int -> NonEmptyList Char -> Bool)

--randomiseList
-- GIVEN a random seed
-- AND a list of characters
-- WHEN I randomise the list
checkRandomiseList = do
    checkRandomiseListLength
    checkRandomiseListObjects

-- THEN I recieve a list of the same length
prop_randomiseList_len seed xs = length (randomiseList seed xs) == length xs
checkRandomiseListLength = quickCheck (prop_randomiseList_len :: Int -> [Char] -> Bool)

-- AND the list contains the same objects
prop_randomiseList_objects seed xs = sort (randomiseList seed xs) == sort xs
checkRandomiseListObjects = quickCheck (prop_randomiseList_objects :: Int -> [Char] -> Bool)

-- APPLICATION ----------------------------------------------------------------
checkApplication = do
    -- javacpp_init
    checkApplyPopulation

--applyPopulation
-- GIVEN a non-empty heuristic population
-- WHEN the population is applied
checkApplyPopulation = do
    checkApplyPopulationLength
    checkApplyPopulationRounds
    checkApplyPopulationHeuristics

-- THEN the resulting population is of the same length
prop_applyPopulation_len (NonEmpty hs) = length (applyPopulation hs) == length hs
checkApplyPopulationLength = quickCheck prop_applyPopulation_len

-- AND each heuristic's round counter has increased by one
prop_applyPopulation_rounds seed (NonEmpty hs) = snd (snd ((applyPopulation hs) !! i)) == (snd (snd (hs !! i))) + 1
    where i = getRandomIndex seed hs
checkApplyPopulationRounds = quickCheck prop_applyPopulation_rounds

-- AND the heuristics have not been modified
prop_applyPopulation_heuristics seed (NonEmpty hs) = fst ((applyPopulation hs) !! i) == fst (hs !! i)
    where i = getRandomIndex seed hs
checkApplyPopulationHeuristics = quickCheck prop_applyPopulation_heuristics
