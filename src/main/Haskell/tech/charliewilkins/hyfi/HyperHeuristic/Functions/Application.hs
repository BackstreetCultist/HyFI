module Application where

import Control.Monad.State

import HyperHeuristicTypes
import RandomOperators (heuristicToSeed)
import Solution (applyHeuristicRepresentationToPopulation, SolutionPopulation)

applyPopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
applyPopulation set = do
                    ss <- get
                    let hs' = evalState set ss
                    if hs' == [] then
                        return hs'
                    else do
                        let ((h, (s,r)):hs) = hs'
                        let x = unzip (applyHeuristicRepresentationToPopulation h ss)
                        put (fst x)
                        return $! (h, (s + avg (snd x), r+1)) : (evalState (applyPopulation (return hs)) (fst x))

avg :: [Int] -> Int
avg [] = 0
avg xs = (sum xs) `div` (length xs)
