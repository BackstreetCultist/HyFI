module Solution.PermutationFlowShop.Types where

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Instance -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Instance -> Int
                    --Old -> New
type Acceptor = Solution -> Solution -> Instance -> Bool
type BuiltHeuristic = (Operator, OperatorMagnitude, Acceptor, Evaluator)

type Solution = [Int]
-- A list of jobs in the order they will be processed
type Instance = [[Int]]
-- A list of machines, with each machine represented by the time each job requires it 
