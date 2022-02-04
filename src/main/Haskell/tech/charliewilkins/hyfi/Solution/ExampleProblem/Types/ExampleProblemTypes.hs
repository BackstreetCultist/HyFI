module ExampleProblemTypes where

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Int
type Acceptor = Solution -> Solution -> Bool
type Heuristic = (Operator, OperatorMagnitude, Evaluator, Acceptor)

type Solution = [Char]
