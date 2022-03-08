module Solution.BinPacking.Types.BinPackingTypes where

import Data.List.Unique (repeated)

type HeuristicRepresentation = [Char]
type Operator = Solution -> OperatorMagnitude -> Instance -> Solution
type OperatorMagnitude = Int
type OperatorClass = [Operator]
type Evaluator = Solution -> Solution -> Instance -> Int
                    --Old -> New
type Acceptor = Solution -> Solution -> Instance -> Bool
type BuiltHeuristic = (Operator, OperatorMagnitude, Acceptor, Evaluator)

type Solution = [[Int]]
-- So 0 being in the 0th set means that Object 0 is in Bin 0
-- Some properties of this ordering:
  -- The length of the inner set concatenated together should equal the number of objects
property1 :: Solution -> Instance -> Bool
property1 s i = length (concat s) == length (snd i)
  -- The inner set concatenated together should be a valid set i.e. contain no duplicates
property2 :: Solution -> Instance -> Bool
property2 s _ = repeated (concat s) == []
  -- The sum of the weights of objects in any inner set should not exceed the weight limit
property3 :: Solution -> Instance -> Bool
property3 [] _ = True
property3 (b:bs) i = (fst i) >= sum [(snd i) !! x | x <- b] && property3 bs i
  -- The length of the outer set is the number of bins

type Instance = (Double, [Double])
-- Where the first value is the weight limit of a bin, and the second is the list of bins

solutionValid :: Solution -> Instance -> Bool
solutionValid s i = (property1 s i) && (property2 s i) && (property3 s i)
