module HyFI where

import Control.Monad.State

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.HyperHeuristic (generateHeuristicPopulationOfSize, runHeuristic)
import HyperHeuristic.Types.HyperHeuristicTypes (HeuristicPopulation)
import Solution.Solution (getInstance, generateSolutionPopulationOfSize, evaluateSolution, SolutionPopulation)

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
                                                                                            print "*** NEW SOLUTION VALUES ***"
                                                                                            print (map (\x -> evaluateSolution x (fst ss)) (snd ss))
                                                                                            print "***********************************"
                                                                                            print ()

                                                                                            coreLoop (attach hs ss) ss startTime newTime limit
                                                    | otherwise = do
                                                        return (detach set initialSs)

main s t = do
    let i = getInstance "hidden-k3-s0-r5-n700-01-S2069048075.sat05-488.reshuffled-07.txt"
    let initialSolutionPopulation = generateSolutionPopulationOfSize 8 s i
    let initialHeuristicPopulation = generateHeuristicPopulationOfSize 8 (s+1)

    print "*** INITIAL HEURISTIC POPULATION ***"
    print initialHeuristicPopulation
    print "************************************"
    print "*** INITIAL SOLUTION POPULATION ***"
    print (map (\x -> evaluateSolution x (fst initialSolutionPopulation)) (snd initialSolutionPopulation))
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
    print (snd finalSolutionPopulation)
    print "*********************************"
    print "*** FINAL SOLUTION VALUES ***"
    print (map (\x -> evaluateSolution x (fst finalSolutionPopulation)) (snd finalSolutionPopulation))
    print "*****************************"
    print "Bye!"
    print "..."
    print "Hang on"
    print "One final check"
    print ((fst finalSolutionPopulation) == i)
