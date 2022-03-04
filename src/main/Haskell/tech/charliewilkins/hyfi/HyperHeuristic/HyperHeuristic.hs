{-# LANGUAGE ForeignFunctionInterface #-}
module HyperHeuristic.HyperHeuristic where

import Control.Monad.State

import System.Random (randomRs, mkStdGen)

import HyperHeuristic.Types.HyperHeuristicTypes
import HyperHeuristic.Functions.Application (applyPopulation)
import HyperHeuristic.Functions.Evolution (evolvePopulation)
import Solution.Solution (SolutionPopulation)

-- STARTUP --------------------------------------------------------------------

generateHeuristic :: Int -> Heuristic
generateHeuristic seed = take 16 (randomRs ('0', '1') (mkStdGen seed))

generateHeuristicPopulationOfSize :: Int -> Int -> HeuristicPopulation
generateHeuristicPopulationOfSize 0 _ = []
generateHeuristicPopulationOfSize n seed = (generateHeuristic seed, (0, 0)) : generateHeuristicPopulationOfSize (n-1) (seed+1)

-- RUNNING --------------------------------------------------------------------

runHeuristic :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
runHeuristic set = evolvePopulation (applyPopulation set)
