module Solution.BooleanSatisfiability.BooleanSatisfiability where

import System.Directory
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRs, mkStdGen)

import Solution.BooleanSatisfiability.Types.BooleanSatisfiabilityTypes
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityOperators (flipRandomVariable, flipRandomVariableFromBrokenClause)
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityAcceptors (naiveAcceptor, improvingAcceptor)
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityEvaluators (newObjectiveValue, improvement)

generator :: Int -> Instance -> Solution
generator s i = take (fst i) (randomRs ('0', '1') (mkStdGen s))

getOperatorsByClass :: [[Operator]]
getOperatorsByClass = [[flipRandomVariable, flipRandomVariableFromBrokenClause]]

getAcceptors :: [Acceptor]
getAcceptors = [naiveAcceptor, improvingAcceptor]

getEvaluators :: [Evaluator]
getEvaluators = [newObjectiveValue, improvement]

getProblemInstance :: String -> Instance
getProblemInstance file = unsafePerformIO (loadProblemInstance file)

loadProblemInstance :: String -> IO (Instance)
loadProblemInstance file = do
                        let filePath =  "Solution/BooleanSatisfiability/Instances/" ++ file
                        print filePath
                        handle <- openFile filePath ReadMode
                        contents <- hGetContents handle
                        putStr contents
                        let vars = getNumberOfVars (lines contents)
                        let clauses = getClauses (lines contents)
                        hClose handle
                        return (vars,clauses)

getNumberOfVars :: [String] -> Int
getNumberOfVars (line:lines) | (head line) == 'p' = read ((words line) !! 2) :: Int
                             | otherwise = getNumberOfVars lines

getClauses :: [String] -> [[Int]]
getClauses [] = []
getClauses (line:lines) | ((head line) /= 'p' && (head line /= 'c')) = [init (map read (words line) :: [Int])] ++ getClauses lines
                        | otherwise = getClauses lines

getObjectiveValue :: Evaluator
getObjectiveValue = newObjectiveValue
