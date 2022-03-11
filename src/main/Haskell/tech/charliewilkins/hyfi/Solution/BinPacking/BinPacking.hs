module Solution.BinPacking.BinPacking where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.Functions.Helpers.RandomOperators (randomiseList)
import Solution.BinPacking.Types.BinPackingTypes
import Solution.BinPacking.Functions.BinPackingEvaluators (newObjectiveValue)

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
                    let filePath =  "Solution/BinPacking/Instances/" ++ file
                    print filePath
                    handle <- openFile filePath ReadMode
                    contents <- hGetContents handle
                    putStr contents
                    let limit = read (head (words (lines contents !! 2)))
                    let clauses = [read (head (words x)) | x <- drop 3 (lines contents)]
                    hClose handle
                    return (limit,clauses)

-- getOperatorsByClass :: [[Operator]]

-- getEvaluators :: [Evaluator]

-- getAcceptors :: [Acceptor]

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
