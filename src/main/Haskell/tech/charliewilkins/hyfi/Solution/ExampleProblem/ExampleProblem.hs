module ExampleProblem where

import System.Random (randomRs, mkStdGen)

import Data.Char (digitToInt)
import Data.List (foldl')

import ExampleProblemTypes
import ExampleProblemOperators (randomBitFlip)
import ExampleProblemAcceptors (naiveAcceptor, improvingAcceptor)
import ExampleProblemEvaluators (newObjectiveValue, improvement, valueDistance, searchSpaceDistance)

generator :: Int -> Solution
generator seed = take 8 (randomRs ('0', '1') (mkStdGen seed))

-- Remember that anything mod n is 0 and n-1 inclusive
-- And that magnitude should be at least 1
buildHeuristic :: HeuristicRepresentation -> Heuristic
buildHeuristic h = (
                    (operators !! ((length operators `mod` (binaryVal opBits)))),
                    ((binaryVal magBits) + 1),
                    (acceptors !! ((length acceptors) `mod` (binaryVal accBits))),
                    (evaluators !! ((length evaluators) `mod` (binaryVal evalBits)))
                   )
                    where
                        operators = (opClasses !! ((length opClasses) `mod` (binaryVal classBits)))
                        opClasses = [[randomBitFlip]]
                        acceptors = [newObjectiveValue, improvement, valueDistance, searchSpaceDistance]
                        evaluators = [naiveAcceptor, improvingAcceptor]
                        opBits = substring 0 3 h
                        classBits = substring 3 6 h
                        magBits = substring 6 9 h
                        accBits = substring 9 12 h
                        evalBits = substring 12 15 h

-- Substring between start (inclusive) and end (exclusive)
substring :: Int -> Int -> [a] -> [a]
substring start end xs = take (end - start) (drop start xs)

binaryVal :: String -> Int
binaryVal xs = foldl' (\acc x -> acc * 2 + digitToInt x) 0 xs
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell

-- runHeuristic :: Heuristic -> Solution -> (Solution, Int)
