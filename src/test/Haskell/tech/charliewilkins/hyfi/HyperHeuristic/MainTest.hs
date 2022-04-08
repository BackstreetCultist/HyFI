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
    checkSelectParents
    checkGenerateChildren

-- reproductionStep
-- GIVEN a heuristic population
-- WHEN I call reproductionStep
checkReproductionStep = do
    checkReproductionStepLength
-- THEN I recieve a population of length 2
prop_checkReproduction_len hPop = length (reproductionStep hPop) == 2
checkReproductionStepLength = quickCheck prop_checkReproduction_len

-- selectParents
-- GIVEN a heuristic population
-- WHEN I select parents
checkSelectParents = do
    checkSelectParentsDifferent
-- AND the list is of size > 4
-- AND the first entry in the list is valid binary
-- THEN I receive two different parents
prop_checkSelectParents_diff hPop =  length hPop > 2 && all (== True) (map (\x -> fst x /= "" && snd (snd x) >= 0) hPop) ==> fst parents /= snd parents where parents = (selectParents ([("10001110101",(0,0))] ++ hPop))
checkSelectParentsDifferent = quickCheckWith stdArgs { maxDiscardRatio = 1000 } prop_checkSelectParents_diff

-- generateChildren
-- GIVEN a pair of heuristics
-- WHEN I call generateChildren
checkGenerateChildren = do
    checkGenerateChildrenLength
    checkGenerateChildrenChildLength
    checkGenerateChildrenUnique
    checkGenerateChildrenDifferent
-- THEN I recieve a population of length 2
prop_checkGenerateChildren_len (x,y) = length (generateChildren ((generateHeuristic x),(generateHeuristic y))) == 2
checkGenerateChildrenLength = quickCheck prop_checkGenerateChildren_len
-- AND each child is the correct length
prop_generateChildren_childLen (x,y) = length (fst (head (generateChildren ((generateHeuristic x),(generateHeuristic y))))) == length (generateHeuristic x) && length (fst (head (reverse (generateChildren ((generateHeuristic x),(generateHeuristic y)))))) == length (generateHeuristic x)
checkGenerateChildrenChildLength = quickCheck prop_generateChildren_childLen
-- AND the two parents are different
-- THEN each child is unique
prop_checkGenerateChildren_unq y = length y > 1 ==> fst (head children) /= fst (last children) where children = generateChildren ("01",y)
checkGenerateChildrenUnique = quickCheck prop_checkGenerateChildren_unq
-- AND the two parents are different
-- THEN neither child is equal to a parent
prop_checkGenerateChildren_diff y = length y > 1 && y /= "01" ==> (fst (head children) /= "01" || fst (head children) /= y) && (fst (last children) /= "01" || fst (last children) /= y) where children = generateChildren ("01",y)
checkGenerateChildrenDifferent = quickCheck prop_checkGenerateChildren_diff

checkMutation = do
    return ()

-- GENERAL --------------------------------------------------------------------
checkHyperHeuristic = do
    checkStartup
    checkEvolution
