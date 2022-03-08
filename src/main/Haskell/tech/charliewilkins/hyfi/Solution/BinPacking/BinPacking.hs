module Solution.BinPacking.BinPacking where

import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Solution.BinPacking.Types.BinPackingTypes

-- generator :: Int -> Instance -> Solution

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

-- getNumberOfVars :: [String] -> Int
-- getNumberOfVars (line:lines) | (head line) == 'p' = read ((words line) !! 2) :: Int
--                              | otherwise = getNumberOfVars lines

-- getClauses :: [String] -> [[Int]]
-- getClauses [] = []
-- getClauses (line:lines) | ((head line) /= 'p' && (head line /= 'c')) = [init (map read (words line) :: [Int])] ++ getClauses lines
--                         | otherwise = getClauses lines

-- getOperatorsByClass :: [[Operator]]

-- getEvaluators :: [Evaluator]

-- getAcceptors :: [Acceptor]

-- getObjectiveValue :: Evaluator
