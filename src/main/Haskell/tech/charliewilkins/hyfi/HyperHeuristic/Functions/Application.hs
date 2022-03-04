module HyperHeuristic.Functions.Application where

import Control.Monad.State

import HyperHeuristic.Types.HyperHeuristicTypes
import HyperHeuristic.Functions.Helpers.RandomOperators (heuristicToSeed)
import Solution.Solution (applyHeuristicRepresentationToPopulation, SolutionPopulation)

applyPopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
applyPopulation set = do
                    (i, ss) <- get
                    let hs' = evalState set (i, ss)
                    if hs' == [] then
                        return hs'
                    else do
                        let ((h, (s,r)):hs) = hs'
                        let x = unzip (applyHeuristicRepresentationToPopulation h (i, ss))
                        put (i, (fst x))
                        return $! (h, (s + avg (snd x), r+1)) : (evalState (applyPopulation (return hs)) (i, (fst x)))

avg :: [Int] -> Int
avg [] = 0
avg xs = (sum xs) `div` (length xs)
