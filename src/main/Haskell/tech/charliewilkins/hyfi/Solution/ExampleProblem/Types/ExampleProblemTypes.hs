module Solution.ExampleProblem.Types.ExampleProblemTypes where

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Int
type Acceptor = Solution -> Solution -> Bool
type BuiltHeuristic = (Operator, OperatorMagnitude, Acceptor, Evaluator)

type Solution = [Char]
