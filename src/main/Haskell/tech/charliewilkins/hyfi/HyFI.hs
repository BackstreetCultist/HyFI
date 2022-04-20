import Control.DeepSeq (deepseq)
import Control.Monad.State

import Data.Function (on)
import Data.List (sortBy)
import Data.List.Unique (sortUniq)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

import HyperHeuristic.Main (generateHeuristicPopulationOfSize, evolvePopulation)
import HyperHeuristic.Types (HeuristicPopulation)
import Helpers.RandomOperators (randomiseList, getSeed)
import Solution.Main (SolutionPopulation, getInstance, generateSolutionPopulationOfSize, evaluateSolution, applyPopulation)

attach :: HeuristicPopulation -> SolutionPopulation -> State SolutionPopulation HeuristicPopulation
attach hs ss = return hs

detach :: SolutionPopulation -> State SolutionPopulation HeuristicPopulation -> (HeuristicPopulation, SolutionPopulation)
detach sPop set = runState set sPop

coreLoop :: State SolutionPopulation HeuristicPopulation -> SolutionPopulation -> UTCTime -> UTCTime -> NominalDiffTime -> IO ((HeuristicPopulation, SolutionPopulation))
coreLoop set initialSs startTime currentTime limit  | ((diffUTCTime currentTime startTime) <= limit) = do
                                                        newTime <- getCurrentTime
                                                        let (hs, ss) = detach initialSs $! cyclePopulation set

                                                        deepseq (hs, ss) print ()
                                                        let ss' = ((fst ss), (randomiseList (getSeed (fst (head hs))) (snd ss)))

                                                        coreLoop (attach hs ss') ss' startTime newTime limit
                                                    | otherwise = do
                                                        return (detach initialSs (applyPopulation set))

main = do
    args <- getArgs
    let f = args !! 0
    let sS = args !! 1
    let s = read sS :: Int
    let tS = args !! 2
    let t = fromInteger (read tS :: Integer)
    let i = getInstance f

    let initialHeuristicPopulation = generateHeuristicPopulationOfSize 8 (s+1)
    let initialSolutionPopulation = generateSolutionPopulationOfSize ((length initialHeuristicPopulation)*2) s i

    print "*** INITIAL HEURISTIC POPULATION ***"
    print initialHeuristicPopulation
    print "************************************"
    print "*** INITIAL SOLUTION VALUES ***"
    print ((map.map) (\y -> evaluateSolution y i) (snd initialSolutionPopulation))
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
    print "*** FINAL SOLUTION VALUES ***"
    let solutionsWithValues = sortBy (compare `on` snd) [(x, evaluateSolution x i) | x <- concat (snd finalSolutionPopulation)]
    print solutionsWithValues
    print "*****************************"
    print (snd (head (reverse solutionsWithValues)))

avg :: [Int] -> Int
avg [] = 0
avg xs = (sum xs) `div` (length xs)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
-- https://stackoverflow.com/questions/13190314/io-happens-out-of-order-when-using-getline-and-putstr

--Wraps application and evolution into one function
cyclePopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
cyclePopulation set = do
                    hPop <- applyPopulation set
                    return (evolvePopulation hPop)
