{-# LANGUAGE ScopedTypeVariables #-}

module ProblemModules.BinPacking.MainTest where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck

import ProblemModules.BinPacking.Main
import ProblemModules.BinPacking.Types
import ProblemModules.BinPacking.Functions.Operators
import ProblemModules.BinPacking.Functions.Acceptors
import ProblemModules.BinPacking.Functions.Evaluators

getInstance :: Instance
getInstance = unsafePerformIO (loadInstance "src/main/Haskell/tech/charliewilkins/hyfi/ProblemModules/BinPacking/Instances/schoenfieldhard1.txt")

loadInstance :: String -> IO(Instance)
loadInstance file = do
                    let filePath =  file
                    print filePath
                    handle <- openFile filePath ReadMode
                    contents <- hGetContents handle
                    putStr contents
                    let limit = read (head (words (lines contents !! 2)))
                    let clauses = [read (head (words x)) | x <- drop 3 (lines contents)]
                    hClose handle
                    return (limit,clauses)

-- STARTUP --------------------------------------------------------------------
checkStartup :: IO ()
checkStartup = do
    checkGenerator

-- generator
-- GIVEN a problem instance
-- WHEN I call to generate a solution
checkGenerator = do
    checkGeneratorValid
-- THEN the generated solution is valid
prop_checkGenerator_val seed = solutionValid (generator seed getInstance) getInstance
checkGeneratorValid = quickCheck prop_checkGenerator_val

-- OPERATORS ------------------------------------------------------------------
checkOperators = do
    checkRandomPlace
    checkMaximisingPlace
    checkRepackLowestFilled
    checkDestroyHighestBins
    checkDestroyLowestBins
    checkDestroyRandomBins

-- randomPlace
-- GIVEN a solution
-- WHEN I modify it using the random place algorithm
checkRandomPlace = do
    checkRandomPlaceValid
-- THEN the generated solution is valid
prop_checkRandomPlace_val seed = solutionValid (randomPlace (generator seed getInstance) 1 getInstance) getInstance
checkRandomPlaceValid = quickCheck prop_checkRandomPlace_val

-- maximisingPlace
-- GIVEN a solution
-- WHEN I modify it using the maximising place algorithm
checkMaximisingPlace = do
    checkMaximisingPlaceValid
-- THEN the generated solution is valid
prop_checkMaximisingPlace_val seed = solutionValid (maximisingPlace (generator seed getInstance) 1 getInstance) getInstance
checkMaximisingPlaceValid = quickCheck prop_checkMaximisingPlace_val

-- repackLowestFilled
-- GIVEN a solution
-- WHEN I modify it using the repackLowestFilled algorithm
checkRepackLowestFilled = do
    checkRepackLowestFilledValid
-- THEN the generated solution is valid
prop_checkRepackLowestFilled_val seed = solutionValid (repackLowestFilled (generator seed getInstance) 1 getInstance) getInstance
checkRepackLowestFilledValid = quickCheck prop_checkRepackLowestFilled_val

-- destroyHighestBins
-- GIVEN a solution
-- WHEN I modify it using the destroyHighestBins algorithm
checkDestroyHighestBins = do
    checkDestroyHighestBinsValid
-- THEN the generated solution is valid
prop_checkDestroyHighestBins_val seed = solutionValid (destroyHighestBins (generator seed getInstance) 1 getInstance) getInstance
checkDestroyHighestBinsValid = quickCheck prop_checkDestroyHighestBins_val

-- destroyLowestBins
-- GIVEN a solution
-- WHEN I modify it using the destroyHighestBins algorithm
checkDestroyLowestBins = do
    checkDestroyLowestBinsValid
-- THEN the generated solution is valid
prop_checkDestroyLowestBins_val seed = solutionValid (destroyLowestBins (generator seed getInstance) 1 getInstance) getInstance
checkDestroyLowestBinsValid = quickCheck prop_checkDestroyLowestBins_val

-- destroyRandomBins
-- GIVEN a solution
-- WHEN I modify it using the destroyHighestBins algorithm
checkDestroyRandomBins = do
    checkDestroyRandomBinsValid
-- THEN the generated solution is valid
prop_checkDestroyRandomBins_val seed = solutionValid (destroyRandomBins (generator seed getInstance) 1 getInstance) getInstance
checkDestroyRandomBinsValid = quickCheck prop_checkDestroyRandomBins_val

-- ACCEPTORS ------------------------------------------------------------------
checkAcceptors = do
    return ()

-- EVALUATORS -----------------------------------------------------------------
checkEvaluators = do
    return ()

-- GENERAL --------------------------------------------------------------------
checkBinPacking = do
    checkStartup
    checkOperators
    checkAcceptors
    checkEvaluators
