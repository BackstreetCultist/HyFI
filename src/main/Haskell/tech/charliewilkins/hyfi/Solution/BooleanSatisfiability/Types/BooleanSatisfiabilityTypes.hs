module BooleanSatisfiabilityTypes where

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Instance -> Int
                    --Old -> New
type Acceptor = Solution -> Solution -> Bool
type BuiltHeuristic = (Operator, OperatorMagnitude, Acceptor, Evaluator)

type Solution = [Bool]
type Instance = (Int, [[Int]])
-- Number of variables followed by set of clauses
