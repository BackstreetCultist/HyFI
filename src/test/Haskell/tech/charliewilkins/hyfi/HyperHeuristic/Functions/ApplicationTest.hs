module ApplicationTest where

import Test.QuickCheck

import RandomOperators (getRandomIndex)
import Application

checkApplication = do
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
