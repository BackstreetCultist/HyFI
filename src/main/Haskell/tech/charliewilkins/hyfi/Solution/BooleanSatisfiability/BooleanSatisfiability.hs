module Solution.BooleanSatisfiability.BooleanSatisfiability where

import System.Directory
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRs, mkStdGen)

import Solution.BooleanSatisfiability.Types.BooleanSatisfiabilityTypes
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityOperators (flipRandomVariable, flipRandomVariableFromBrokenClause, reverseSolution, invertSolution)
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityAcceptors (naiveAcceptor, improvingAcceptor, improvingOrChanceAcceptor, substantialImprovementAcceptor)
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityEvaluators (newObjectiveValue, percentageImprovement, improvement, searchSpaceDistance, magnitudeDistance)

generator :: Int -> Instance -> Solution
generator s i = take (fst i) (randomRs ('0', '1') (mkStdGen s))

getOperatorsByClass :: [[Operator]]
getOperatorsByClass = [[flipRandomVariable, flipRandomVariableFromBrokenClause], [reverseSolution, invertSolution]]

getAcceptors :: [Acceptor]
getAcceptors = [improvingAcceptor, improvingOrChanceAcceptor, substantialImprovementAcceptor]

-- All evaluators given *as* evaluators compare two solutions
getEvaluators :: [Evaluator]
getEvaluators = [percentageImprovement, improvement, searchSpaceDistance, magnitudeDistance]

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
