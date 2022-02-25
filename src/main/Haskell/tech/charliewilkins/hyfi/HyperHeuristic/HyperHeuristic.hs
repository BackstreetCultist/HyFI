{-# LANGUAGE ForeignFunctionInterface #-}
module HyperHeuristic where

import System.Random (randomRs, mkStdGen)

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

import HyperHeuristicTypes
import Application (applyPopulation)
import Evolution (evolvePopulation)

-- STARTUP --------------------------------------------------------------------

generateHeuristic :: Int -> Heuristic
generateHeuristic seed = take 16 (randomRs ('0', '1') (mkStdGen seed))

generateHeuristicPopulationOfSize :: Int -> Int -> HeuristicPopulation
generateHeuristicPopulationOfSize 0 _ = []
generateHeuristicPopulationOfSize n seed = (generateHeuristic seed, (0, 0)) : generateHeuristicPopulationOfSize (n-1) (seed+1)
