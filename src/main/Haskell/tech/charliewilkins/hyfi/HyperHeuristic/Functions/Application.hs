module Application where

import HyperHeuristicTypes
import RandomOperators (heuristicToSeed)

applyPopulation :: HeuristicPopulation -> HeuristicPopulation
applyPopulation hs = [(h, ((s + (applyHeuristic h)), (r+1))) | (h, (s, r)) <- hs]

-- apply a heuristic and get back a score for that run
-- this is where we call in to the solution layer
applyHeuristic :: Heuristic -> Int
applyHeuristic =  heuristicToSeed
-- TODO replace with call to solution layer
