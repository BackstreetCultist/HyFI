module HyFI where

import Control.Monad.State

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic (generateHeuristicPopulationOfSize, runHeuristic)
import HyperHeuristicTypes (HeuristicPopulation)
import Solution (generateSolutionPopulationOfSize, evaluateSolution, SolutionPopulation)

attach :: HeuristicPopulation -> SolutionPopulation -> State SolutionPopulation HeuristicPopulation
attach hs ss = return hs

detach :: State SolutionPopulation HeuristicPopulation -> SolutionPopulation -> (HeuristicPopulation, SolutionPopulation)
detach set sPop = runState set sPop

coreLoop :: State SolutionPopulation HeuristicPopulation -> SolutionPopulation -> UTCTime -> UTCTime -> NominalDiffTime -> IO ((HeuristicPopulation, SolutionPopulation))
coreLoop set initialSs startTime currentTime limit  | ((diffUTCTime currentTime startTime) <= limit) = do
                                                                                            newTime <- getCurrentTime
                                                                                            let (hs, ss) = detach (runHeuristic set) initialSs

                                                                                            print ()
                                                                                            print "*** NEW HEURISTIC POPULATION ***"
                                                                                            print hs
                                                                                            print "************************************"
                                                                                            print "*** NEW SOLUTION POPULATION ***"
                                                                                            print ss
                                                                                            print "***********************************"
                                                                                            print ()

                                                                                            coreLoop (attach hs ss) ss startTime newTime limit
                                                    | otherwise = do
                                                        return (detach set initialSs)

main s t = do
    let initialSolutionPopulation = generateSolutionPopulationOfSize 8 s
    let initialHeuristicPopulation = generateHeuristicPopulationOfSize 8 (s+1)

    print "*** INITIAL HEURISTIC POPULATION ***"
    print initialHeuristicPopulation
    print "************************************"
    print "*** INITIAL SOLUTION POPULATION ***"
    print initialSolutionPopulation
    print "***********************************"

    startTime <- getCurrentTime
    print "*** LOOPING ***"
    let set = attach initialHeuristicPopulation initialSolutionPopulation
    currentTime <- getCurrentTime
    print "***************"
    (finalHeuristicPopulation, finalSolutionPopulation) <- coreLoop set initialSolutionPopulation startTime currentTime t

    print "*** FINISHED ***"
    print "****************"
    print "*** FINAL HEURISTIC POPULATION ***"
    print finalHeuristicPopulation
    print "**********************************"
    print "*** FINAL SOLUTION POPULATION ***"
    print finalSolutionPopulation
    print "*********************************"
    print "*** FINAL SOLUTION VALUES ***"
    print (map evaluateSolution finalSolutionPopulation)
    print "*****************************"
    print "Bye!"
