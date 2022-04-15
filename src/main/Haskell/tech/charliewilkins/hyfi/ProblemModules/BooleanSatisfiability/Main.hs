module ProblemModules.BooleanSatisfiability.Main where

import System.Directory
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRs, mkStdGen)

import ProblemModules.BooleanSatisfiability.Types
import ProblemModules.BooleanSatisfiability.Functions.Operators (flipRandomVariable, flipRandomVariableFromBrokenClause, gsat, randomlyReinitialise)
import ProblemModules.BooleanSatisfiability.Functions.Acceptors (naiveAcceptor, improvingAcceptor, improvingOrChanceAcceptor, substantialImprovementAcceptor)
import ProblemModules.BooleanSatisfiability.Functions.Evaluators (newObjectiveValue, percentageImprovement, improvement, searchSpaceDistance, magnitudeDistance)

generator :: Int -> Instance -> Solution
generator s i = take (fst i) (randomRs ('0', '1') (mkStdGen s))

getOperatorsByClass :: [[Operator]]
-- getOperatorsByClass = [[flipRandomVariable, flipRandomVariableFromBrokenClause], [gsat], [randomlyReinitialise]]
getOperatorsByClass = [[flipRandomVariable, flipRandomVariableFromBrokenClause], [randomlyReinitialise]]

getAcceptors :: [Acceptor]
getAcceptors = [improvingAcceptor, improvingOrChanceAcceptor]

-- All evaluators given *as* evaluators compare two solutions
getEvaluators :: [Evaluator]
-- getEvaluators = [percentageImprovement, improvement, searchSpaceDistance, magnitudeDistance]
getEvaluators = [improvement, searchSpaceDistance, magnitudeDistance]

getProblemInstance :: String -> Instance
getProblemInstance file = unsafePerformIO (loadProblemInstance file)

loadProblemInstance :: String -> IO (Instance)
loadProblemInstance file = do
                        let filePath =  "ProblemModules/BooleanSatisfiability/Instances/" ++ file
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
