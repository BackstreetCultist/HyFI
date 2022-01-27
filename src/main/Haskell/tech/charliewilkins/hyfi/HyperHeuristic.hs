{-# LANGUAGE ForeignFunctionInterface #-}
module HyperHeuristic where

import System.Random
import System.IO.Unsafe

import Data.List
import Data.Time.Clock

import Foreign.C
import Foreign.Ptr

type Rounds = Int
type Score = Int
type Heuristic = [Char]
type HeuristicPopulation = [(Heuristic, Score, Rounds)]

-- STARTUP --------------------------------------------------------------------
generateHeuristic :: Int -> Heuristic
generateHeuristic seed = take 16 (randomRs ('0', '1') (mkStdGen seed))

generateHeuristicSetOfSize :: Int -> Int -> HeuristicPopulation
generateHeuristicSetOfSize 0 _ = []
generateHeuristicSetOfSize n seed = (generateHeuristic seed, 0, 0) : generateHeuristicSetOfSize (n-1) (seed+1)

-- APPLICATION ----------------------------------------------------------------
applyPopulation :: HeuristicPopulation -> HeuristicPopulation
applyPopulation hPop = []

-- apply a heuristic and get back a score for that run
-- this is where we call in to the solution layer
applyHeuristic :: Heuristic -> Int
applyHeuristic h = 0

-- EVOLUTION ------------------------------------------------------------------
-- Control function for this section
evolvePopulation :: HeuristicPopulation -> HeuristicPopulation
evolvePopulation hPop = []
-- Produces a new population of length equal to the original

selectParents :: HeuristicPopulation -> (Heuristic, Heuristic)
selectParents hPop = ([], [])
-- This assumes two parents - should we generalise to crossover between n parents?

mutateHeuristic :: Heuristic -> Heuristic
mutateHeuristic h = []

-- JAVA INTERFACE -------------------------------------------------------------
-- foreign import ccall "JavaCPP_init" c_javacpp_init :: CInt -> Ptr (Ptr CString) -> IO ()
-- javacpp_init :: IO ()
-- javacpp_init = c_javacpp_init 0 nullPtr

-- foreign import ccall "runHeuristic" c_runHeuristic :: CString -> CInt
-- runHeuristic :: Heuristic -> Int
-- runHeuristic h = fromIntegral (c_runHeuristic (unsafePerformIO(newCString h)))
-- I think I have to free the C vars afterwards?

-- CONTROL --------------------------------------------------------------------
-- At some future point this may be removed to the Control Layer
main :: Int -> IO ()
main seed = do
    -- javacpp_init
    let hPop = generateHeuristicSetOfSize 8 seed
    startTime <- getCurrentTime
    print hPop
    currentTime <- getCurrentTime
    print("Starting loop")
    newPop <- coreLoop hPop startTime currentTime 5
    print("Exited loop")
    print newPop

coreLoop :: HeuristicPopulation -> UTCTime -> UTCTime -> NominalDiffTime -> IO (HeuristicPopulation)
coreLoop hPop startTime currentTime limit | ((diffUTCTime currentTime startTime) <= limit) = do
                                                                                        newTime <- getCurrentTime
                                                                                        let newPop = evolvePopulation (applyPopulation hPop)
                                                                                        coreLoop newPop startTime newTime limit
                                          | otherwise = do
                                                return hPop
