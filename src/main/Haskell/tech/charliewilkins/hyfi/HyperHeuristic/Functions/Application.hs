module Application where

import Control.Monad.State

import HyperHeuristicTypes
import RandomOperators (heuristicToSeed)
import Solution (applyHeuristicRepresentationToPopulation, SolutionPopulation)

applyPopulation :: (HeuristicPopulation, SolutionPopulation) -> (HeuristicPopulation, SolutionPopulation)
-- -- applyPopulation (hs, ss) = [(h, ((s + (applyHeuristic h)), (r+1))) | (h, (s, r)) <- hs]
applyPopulation ([], ss) = ([],ss)
-- applyPopulation ((h, (s,r):hs, ss) = ((h, ((s + s'), (r+1))), ss') : applyPopulation (hs,ss')
applyPopulation ((h, (s,r)):hs, ss) = ((h, ((s + s'), (r+1))) : hs', ss'')
                            where
                                (hs', ss'') = applyPopulation (hs,ss')
                                (ss', s') = applyHeuristic h ss

-- apply a heuristic and get back a score for that run
-- this is where we call in to the solution layer
-- the score is calculated as the average of the scores recieved from that layer
applyHeuristic :: Heuristic -> SolutionPopulation -> (SolutionPopulation, Int)
applyHeuristic h ss =  (fst x, avg (snd x))
                    where
                        x = unzip (applyHeuristicRepresentationToPopulation h ss)

avg :: [Int] -> Int
avg [] = 0
avg xs = (sum xs) `div` (length xs)
