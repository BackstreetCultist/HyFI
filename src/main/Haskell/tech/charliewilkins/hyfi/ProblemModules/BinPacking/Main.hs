module ProblemModules.BinPacking.Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Helpers.RandomOperators (randomiseList)
import ProblemModules.BinPacking.Types
import ProblemModules.BinPacking.Functions.Acceptors (improvingAcceptor, improvingOrChanceAcceptor, averageFullnessImprovingAcceptor, fullBinsImprovingAcceptor)
import ProblemModules.BinPacking.Functions.Evaluators (newObjectiveValue, improvement, fullBinsImprovement, searchSpaceDistance, averageFullnessIncrease)
import ProblemModules.BinPacking.Functions.Operators (randomPlace, maximisingPlace, repackLowestFilled, destroyHighestBins, destroyLowestBins, destroyRandomBins)

generator :: Int -> Instance -> Solution
generator seed i = nextFit [[]] (randomiseList seed (take (length (snd i)) [0..])) i

nextFit :: Solution -> [Int] -> Instance -> Solution
nextFit s [] _ = s
nextFit [[]] (x:xs) i = nextFit [[x]] xs i
nextFit s (x:xs) i | ((sum (map (\z -> (snd i) !! z) (last s))) + ((snd i) !! x)) > (fst i) = nextFit (s ++ [[x]]) xs i
                   | otherwise = nextFit (reverse (tail (reverse s)) ++ [(last s) ++ [x]]) xs i

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

getAcceptors :: [Acceptor]
getAcceptors = [improvingAcceptor, improvingOrChanceAcceptor, averageFullnessImprovingAcceptor, fullBinsImprovingAcceptor]

getEvaluators :: [Evaluator]
getEvaluators = [improvement, fullBinsImprovement, searchSpaceDistance, averageFullnessIncrease]

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
