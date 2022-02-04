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

-- CONTROL --------------------------------------------------------------------

run :: Int -> NominalDiffTime -> IO ()
run seed limit = do
    -- javacpp_init
    let hPop = generateHeuristicPopulationOfSize 8 seed
    startTime <- getCurrentTime
    print hPop
    currentTime <- getCurrentTime
    print("Starting loop")
    newPop <- coreLoop hPop startTime currentTime limit
    print("Exited loop")
    print newPop

coreLoop :: HeuristicPopulation -> UTCTime -> UTCTime -> NominalDiffTime -> IO (HeuristicPopulation)
coreLoop hPop startTime currentTime limit | ((diffUTCTime currentTime startTime) <= limit) = do
                                                                                        newTime <- getCurrentTime
                                                                                        let newPop = evolvePopulation (applyPopulation hPop)
                                                                                        print newPop
                                                                                        coreLoop newPop startTime newTime limit
                                          | otherwise = do
                                                return hPop
