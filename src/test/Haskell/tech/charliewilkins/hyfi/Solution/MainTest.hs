{-# LANGUAGE ScopedTypeVariables #-}

module Solution.MainTest where

import Data.List (isInfixOf)
import Test.QuickCheck

import Solution.Main

-- STARTUP --------------------------------------------------------------------
checkStartup = do
    return ()

-- BUILDING HEURISTIC ---------------------------------------------------------
checkBuilding = do
    checkBinaryToItem
    checkSubstring

-- GIVEN a binary string and a non-empty arbitray list
-- WHEN I attempt to use modulo to retrieve an index
checkBinaryToItem = do
    checkBinaryToItemFirst
    checkBinaryToItemLast
    checkBinaryToItemRotation
    checkBinaryToItemSafe
-- AND the binary string is equal to 0
-- THEN I retrieve the first item in the list
prop_checkBinaryItem_fst (xs :: String) = xs /= "" ==> (binaryToItem "000" xs) == head xs
checkBinaryToItemFirst = quickCheck prop_checkBinaryItem_fst
-- AND the binary string is equal to three
-- AND there are four items in the list
-- THEN I retrieve the last item in the list
prop_checkBinaryItem_lst = forAll (vectorOf 4 arbitrary) $ \ (xs :: String) -> (binaryToItem "011" xs) == head (reverse xs)
checkBinaryToItemLast = quickCheck prop_checkBinaryItem_lst
-- AND the binary string is equal to four
-- AND there are four items in the list
-- THEN I retrieve the first item in the list
prop_checkBinaryToItem_rot = forAll (vectorOf 4 arbitrary) $ \ (xs :: String) -> (binaryToItem "100" xs) == head xs
checkBinaryToItemRotation = quickCheck prop_checkBinaryToItem_rot
-- THEN the item retrieved is in the list (there are no errors)
    -- NOTE that this shortcuts routing the binary to the integer
prop_checkBinaryToItem_safe (xs :: String) y = xs /= "" ==> [xs !! (y `mod` length xs)] `isInfixOf` xs
checkBinaryToItemSafe = quickCheck prop_checkBinaryToItem_safe

-- GIVEN a string and co-ordinates for a substring
-- WHEN I extract the substring
checkSubstring = do
    checkSubstringLength
    checkSubstringAccuracy
-- THEN the substring is of length e-s
prop_checkSubstring_len s e = forAll (vectorOf (e+1) arbitrary) $ \ (xs :: String) -> (s >= 0) && (s < e) ==> length (substring s e xs) == e-s
checkSubstringLength = quickCheck prop_checkSubstring_len
-- AND the substring is a sub-section of the original string
prop_checkSubstring_acc s e = forAll (vectorOf (e+1) arbitrary) $ \ (xs :: String) -> (s >= 0) && (s < e) ==> (substring s e xs) `isInfixOf` xs
checkSubstringAccuracy = quickCheck prop_checkSubstring_acc

-- RUNNING HEURISTIC ----------------------------------------------------------
checkRunning = do
    return ()

-- GENERAL --------------------------------------------------------------------
checkSolution = do
    checkStartup
    checkBuilding
    checkRunning
