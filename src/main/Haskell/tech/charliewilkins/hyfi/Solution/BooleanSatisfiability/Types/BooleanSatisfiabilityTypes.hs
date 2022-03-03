module BooleanSatisfiabilityTypes where

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Instance -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Instance -> Int
                    --Old -> New
type Acceptor = Solution -> Solution -> Bool
type BuiltHeuristic = (Operator, OperatorMagnitude, Acceptor, Evaluator)

type Solution = [Char]
-- We will take 1 to be True and 0 to be False as per convention
type Instance = (Int, [[Int]])
-- Number of variables followed by set of clauses
