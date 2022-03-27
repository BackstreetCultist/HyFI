module ProblemModules.BinPacking.Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Helpers.RandomOperators (randomiseList)
import ProblemModules.BinPacking.Types
import ProblemModules.BinPacking.Functions.Acceptors (naiveAcceptor, improvingAcceptor, improvingOrChanceAcceptor, averageFullnessImprovingAcceptor, fullBinsImprovingAcceptor)
import ProblemModules.BinPacking.Functions.Evaluators (newObjectiveValue, improvement, fullBinsImprovement, searchSpaceDistance, averageFullnessIncrease)
import ProblemModules.BinPacking.Functions.Operators (randomPlace, maximisingPlace, repackLowestFilled, destroyHighestBins, destroyLowestBins, destroyRandomBins)

generator :: Int -> Instance -> Solution
generator seed i = firstFit [[]] (randomiseList seed (take (length (snd i)) [0..])) i

firstFit :: Solution -> [Int] -> Instance -> Solution
firstFit s [] i = s
firstFit s (x:xs) i = firstFit (placeVal s x i ) xs i

placeVal :: Solution -> Int -> Instance -> Solution
placeVal [] y _ = [[y]]
placeVal (x:xs) y i | ((sum (map (\z -> (snd i) !! z) x)) + ((snd i) !! y)) > (fst i) = x : placeVal xs y i
                    | otherwise = (x ++ [y]) : xs

getProblemInstance :: String -> Instance
getProblemInstance file = unsafePerformIO (loadProblemInstance file)

loadProblemInstance :: String -> IO(Instance)
loadProblemInstance file = do
                    let filePath =  "ProblemModules/BinPacking/Instances/" ++ file
                    print filePath
                    handle <- openFile filePath ReadMode
                    contents <- hGetContents handle
                    putStr contents
                    let limit = read (head (words (lines contents !! 2)))
                    let clauses = [read (head (words x)) | x <- drop 3 (lines contents)]
                    hClose handle
                    return (limit,clauses)

getOperatorsByClass :: [[Operator]]
getOperatorsByClass = [[randomPlace, maximisingPlace], [repackLowestFilled], [destroyHighestBins, destroyLowestBins, destroyRandomBins]]
-- getOperatorsByClass = [[repackLowestFilled]]

getEvaluators :: [Evaluator]
getEvaluators = [improvement, fullBinsImprovement, searchSpaceDistance, averageFullnessIncrease]

getAcceptors :: [Acceptor]
getAcceptors = [naiveAcceptor, improvingAcceptor, improvingOrChanceAcceptor, averageFullnessImprovingAcceptor, fullBinsImprovingAcceptor]

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
