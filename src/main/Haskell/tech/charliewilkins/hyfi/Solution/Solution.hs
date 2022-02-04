module Solution where

import HyperHeuristicTypes

generateSolution :: Int -> (Int -> a) -> a
generateSolution seed gen = gen seed

generateSolutionPopulationOfSize :: Int -> Int -> (Int -> a) -> [a]
generateSolutionPopulationOfSize 0 _ _ = []
generateSolutionPopulationOfSize n seed gen = generateSolution seed gen : generateSolutionPopulationOfSize (n-1) (seed+1) gen

-- applyHeuristicToPopulation :: a -> [b] -> (a -> b -> Int) -> Int
-- applyHeuristicToPopulation h ss 
