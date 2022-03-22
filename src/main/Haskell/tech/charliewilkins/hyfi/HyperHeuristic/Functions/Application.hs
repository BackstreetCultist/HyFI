module HyperHeuristic.Functions.Application where

import Control.Monad.State
import Control.Parallel.Strategies (parMap, rdeepseq)

import HyperHeuristic.Types.HyperHeuristicTypes
import HyperHeuristic.Functions.Helpers.RandomOperators (heuristicToSeed)
import Solution.Solution (applyHeuristicRepresentationToSolutions, SolutionPopulation)

applyPopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
applyPopulation set = do
                    (i, sss) <- get
                    let hs = evalState set (i, sss)
                    let results = parMap rdeepseq (\x -> applyHeuristicRepresentationToSolutions (fst (fst x)) i (snd x)) (zip hs sss) -- [[(Solution, Int)]]
                    let hs' = map (\((h, (s,r)), result) -> (h, (s + sum (snd (unzip result)), r+1))) (zip hs results) -- HeuristicPopulation
                    let sss' = (map fst (map unzip results)) -- [[Solution, Int]] -> [[Solution]]
                    put (i, (sss' ++ (drop (length sss') sss))) -- Keep any excess solution sets that were not run
                    return hs'
