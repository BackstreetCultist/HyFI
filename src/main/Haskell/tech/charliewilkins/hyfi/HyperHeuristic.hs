{-# LANGUAGE ForeignFunctionInterface #-}
module HyperHeuristic where

import System.Random
import System.IO.Unsafe

import Data.Char --TODO remove (used currently in applyHeuristic)
import Data.List
import Data.Time.Clock

import Foreign.C
import Foreign.Ptr

type Rounds = Int
type Score = Int
type Heuristic = [Char]
type HeuristicPopulation = [(Heuristic, (Score, Rounds))]

-- STARTUP --------------------------------------------------------------------

generateHeuristic :: Int -> Heuristic
generateHeuristic seed = take 16 (randomRs ('0', '1') (mkStdGen seed))

generateHeuristicPopulationOfSize :: Int -> Int -> HeuristicPopulation
generateHeuristicPopulationOfSize 0 _ = []
generateHeuristicPopulationOfSize n seed = (generateHeuristic seed, (0, 0)) : generateHeuristicPopulationOfSize (n-1) (seed+1)

-- APPLICATION ----------------------------------------------------------------

applyPopulation :: HeuristicPopulation -> HeuristicPopulation
applyPopulation hs = [(h, ((s + (applyHeuristic h)), (r+1))) | (h, (s, r)) <- hs]

-- apply a heuristic and get back a score for that run
-- this is where we call in to the solution layer
applyHeuristic :: Heuristic -> Int
applyHeuristic =  foldl' (\acc x -> acc * 2 + digitToInt x) 0
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
-- TODO replace with call to Java function

-- EVOLUTION ------------------------------------------------------------------

-- Control function for this section
evolvePopulation :: HeuristicPopulation -> HeuristicPopulation
evolvePopulation hPop = take (length hPop) (sortByAverageScore (hPop ++ reproductionStep hPop ++ mutationStep hPop))
-- Produces a new population of length equal to the original
-- Of the heuristics with the best average performance

-- REPRODUCTION
reproductionStep :: HeuristicPopulation -> HeuristicPopulation
reproductionStep hPop = applyChildren (generateChildren (selectParents hPop))

selectParents :: HeuristicPopulation -> (Heuristic, Heuristic)
selectParents hPop = (fst (head sortedPop), fst (sortedPop !! 1)) where sortedPop = sortByAverageScore hPop
-- This assumes two parents - should we generalise to crossover between n parents?
-- Naive selection - take two best-performing

generateChildren :: (Heuristic, Heuristic) -> (Heuristic, Heuristic)
generateChildren (p1, p2) = (p1, p2)

--Produces a HeuristicPopulation of length 2 after running each child once
applyChildren :: (Heuristic, Heuristic) -> HeuristicPopulation
applyChildren (c1, c2) = [(c1, (applyHeuristic c1, 1)), (c2, (applyHeuristic c2, 1))]

-- MUTATION
mutationStep :: HeuristicPopulation -> HeuristicPopulation
mutationStep hPop = applyMutoid (mutateHeuristic (selectHeuristicToMutate hPop))

selectHeuristicToMutate :: HeuristicPopulation -> Heuristic
selectHeuristicToMutate (h:hPop) = fst h

mutateHeuristic :: Heuristic -> Heuristic
mutateHeuristic h = []

applyMutoid :: Heuristic -> HeuristicPopulation
applyMutoid m = [(m, (applyHeuristic m, 1))]

-- HELPERS
-- helper function to sort the population by s/r descending
sortByAverageScore :: HeuristicPopulation -> HeuristicPopulation
sortByAverageScore hPop = reverse (map snd (sort (getAverageScores hPop)))
-- note that in the case that average scores are the same,
-- this will favour those with higher scores & more rounds
-- see sort behaviour on tuples

-- helper function for above
getAverageScores :: HeuristicPopulation -> [(Int, (Heuristic, (Score, Rounds)))]
getAverageScores hPop = [((s `div` r), (h, (s, r))) | (h, (s, r)) <- hPop]

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
main :: Int -> NominalDiffTime -> IO ()
main seed limit = do
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
