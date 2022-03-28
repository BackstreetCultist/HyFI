module Helpers.RandomOperatorsTest where

import Test.QuickCheck

import Data.List (sort)

import Helpers.RandomOperators (getRandomIndex, randomiseList)

checkRandomOperators = do
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
