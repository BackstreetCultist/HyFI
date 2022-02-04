module ExampleProblemEvaluators where

import Data.Char (digitToInt)
import Data.List (foldl')

import ExampleProblemTypes

-- TODO do these need normalising somehow?

newObjectiveValue :: Evaluator
newObjectiveValue _ new = foldl' (\acc x -> acc * 2 + digitToInt x) 0 new

improvement :: Evaluator
improvement old new = (newObjectiveValue old new) - (newObjectiveValue new old)

valueDistance :: Evaluator
valueDistance old new = abs (improvement old new)

searchSpaceDistance :: Evaluator
searchSpaceDistance old new = sum (map (\(x,y) -> (if x == y then 0 else 1)) (zip old new))
