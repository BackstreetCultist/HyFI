import Control.DeepSeq (deepseq)
import Control.Monad.State

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.HyperHeuristic (generateHeuristicPopulationOfSize, runHeuristic)
import HyperHeuristic.Types.HyperHeuristicTypes (HeuristicPopulation)
import Solution.Solution (getInstance, generateSolutionPopulationOfSize, evaluateSolution, SolutionPopulation)

attach :: HeuristicPopulation -> SolutionPopulation -> State SolutionPopulation HeuristicPopulation
attach hs ss = return hs

detach :: SolutionPopulation -> State SolutionPopulation HeuristicPopulation -> (HeuristicPopulation, SolutionPopulation)
detach sPop set = runState set sPop

coreLoop :: State SolutionPopulation HeuristicPopulation -> SolutionPopulation -> UTCTime -> UTCTime -> NominalDiffTime -> IO ((HeuristicPopulation, SolutionPopulation))
coreLoop set initialSs startTime currentTime limit  | ((diffUTCTime currentTime startTime) <= limit) = do
                                                                                            newTime <- getCurrentTime
                                                                                            let (hs, ss) = detach initialSs $! runHeuristic set

                                                                                            deepseq (hs, ss) print ()
                                                                                            print "*** NEW HEURISTIC POPULATION ***"
                                                                                            print hs
                                                                                            print "************************************"
                                                                                            print "*** NEW SOLUTION VALUES ***"
                                                                                            print (map (\x -> evaluateSolution x (fst ss)) (snd ss))
                                                                                            print "***********************************"
                                                                                            print ()

                                                                                            coreLoop (attach hs ss) ss startTime newTime limit
                                                    | otherwise = do
                                                        return (detach initialSs set)

main :: IO ()
main = do
    f <- prompt "File: "
    sS <- prompt "Seed: "
    let s = read sS :: Int
    tS <- prompt "Time Limit: "
    let t = fromInteger (read tS :: Integer)
    let i = getInstance f
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

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
-- https://stackoverflow.com/questions/13190314/io-happens-out-of-order-when-using-getline-and-putstr
