module HyperHeuristic.Functions.Evolution where

import Control.Monad.State

import Data.List (sort)
import Data.List.Unique (sortUniq)

import HyperHeuristic.Types.HyperHeuristicTypes
import HyperHeuristic.Functions.Helpers.RandomOperators (getSeed, randomiseList, getRandomIndex)
import HyperHeuristic.Functions.Application (applyPopulation)
import Solution.Solution (SolutionPopulation)

-- Control function for this section
evolvePopulation :: State SolutionPopulation HeuristicPopulation -> State SolutionPopulation HeuristicPopulation
-- evolvePopulation hPop = survivalStep hPop (reproductionStep hPop) (mutationStep hPop)
evolvePopulation initialSet = do
                    initialHeuristicPopulation <- initialSet
                    let reproductionHeuristicPopulation = reproductionStep initialHeuristicPopulation
                    let mutationHeuristicPopulation = mutationStep initialHeuristicPopulation
                    return (survivalStep initialHeuristicPopulation reproductionHeuristicPopulation mutationHeuristicPopulation)

-- Produces a new population of length equal to the original
-- Of the heuristics with the best average performance

-- SURVIVAL
survivalStep :: HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation
survivalStep hPop childs mutoids = replaceWorst hPop childs mutoids

replaceWorst :: HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation
replaceWorst hPop childs mutoids = (drop (length childs + length mutoids) (reverse (sortByAverageScore hPop))) ++ childs ++ mutoids

-- REPRODUCTION
reproductionStep :: HeuristicPopulation -> HeuristicPopulation
reproductionStep hPop = generateChildren (selectParents hPop)
-- reproductionStep set = do
--                     -- Get children as a list rather than tuple
--                     pop <- set
--                     let children = generateChildren (selectParents pop)
--                     return children

selectParents :: HeuristicPopulation -> (Heuristic, Heuristic)
selectParents hPop = tournamentSelection hPop 3
-- This assumes two parents - should we generalise to crossover between n parents?

naiveSelection :: HeuristicPopulation -> (Heuristic, Heuristic)
naiveSelection hPop = (fst (head sortedPop), fst (sortedPop !! 1)) where sortedPop = sortByAverageScore hPop
-- Naive selection - take two best-performing

-- This implementation of tournament selection based on:
-- https://towardsdatascience.com/genetic-algorithm-a-simple-and-intuitive-guide-51c04cc1f9ed
-- But with average score rather than f(x)
tournamentSelection :: HeuristicPopulation -> Int -> (Heuristic, Heuristic)
tournamentSelection hPop k = 
                            (
                                fst (head (sortByAverageScore (getRandomSublistOfSize k (getSeed (fst (head hPop))) hPop))),
                                fst (head (sortByAverageScore (getRandomSublistOfSize k (getSeed (fst (last hPop))) hPop)))
                            )
-- Note that this has the chance of selecting the same parent in both 'slots' - is this an issue? TODO

-- Returns a random sublist of size k
getRandomSublistOfSize :: Int -> Int -> [a] -> [a]
getRandomSublistOfSize 0 _ _ = []
getRandomSublistOfSize k seed xs = head (randomisedList) : getRandomSublistOfSize (k-1) (seed+1) (tail randomisedList)
                                where
                                    randomisedList = randomiseList seed xs

generateChildren :: (Heuristic, Heuristic) -> HeuristicPopulation
generateChildren (p1, p2) = [(c1, (0,0)), (c2, (0,0))]
                            where
                              (c1, c2) = kPointCrossover (p1, p2) 3 (getSeed p1)

kPointCrossover :: (Heuristic, Heuristic) -> Int -> Int -> (Heuristic, Heuristic)
kPointCrossover x 0 _ = x
kPointCrossover x k seed | (k `mod` 2 == 0) = kPointCrossover x (k-1) (seed+1)
kPointCrossover x k seed | otherwise = onePointCrossover (kPointCrossover x (k-1) (seed+1)) seed

onePointCrossover :: (Heuristic, Heuristic) -> Int -> (Heuristic, Heuristic)
onePointCrossover (h1, h2) seed = (
                                    (take i h1 ++ drop i h2),
                                    (take i h2 ++ drop i h1)
                                  ) where
                                    i = getRandomIndex seed h1

-- MUTATION
mutationStep :: HeuristicPopulation -> HeuristicPopulation
mutationStep hPop = mutateHeuristics (selectHeuristicsToMutate (((getLargestCountOfEquivalents (map fst hPop)) `div` 2)+1) hPop)
-- mutationStep set = do
--                 pop <- set
--                 let maxIdenticalCount = getLargestCountOfEquivalents (map fst pop)
--                 let mutoidCount = (maxIdenticalCount `div` 2) + 1
--                 let mutoids = mutateHeuristics (selectHeuristicsToMutate mutoidCount pop)
--                 return mutoids

-- Gets the size of the largest set of equal values in a list
-- For example
-- ["11111111", "11111111", "11111111", "11110000", "11110000", "11111111", "00000000", "11110000"]
-- returns 4
getLargestCountOfEquivalents :: Eq a => Ord a => [a] -> Int
getLargestCountOfEquivalents xs = head (reverse (sort (map length (map (\y -> filter (==y) xs) (sortUniq xs)))))

selectHeuristicsToMutate :: Int -> HeuristicPopulation -> [Heuristic]
selectHeuristicsToMutate i [] = []
selectHeuristicsToMutate 0 _ = []
selectHeuristicsToMutate i hPop = fst (hPop !! (getRandomIndex (getSeed (fst (head hPop))) hPop)) : selectHeuristicsToMutate (i-1) (tail hPop)

mutateHeuristics :: [Heuristic] -> HeuristicPopulation
mutateHeuristics [] = []
mutateHeuristics (h:hs) = (flipRandomBits (length h) h, (0,0)) : mutateHeuristics hs

flipRandomBits :: Int -> Heuristic -> Heuristic
flipRandomBits 0 h = h
flipRandomBits mag h = flipRandomBits (mag-1) (take i h ++ [bit] ++ drop (i+1) h)
                where
                    bit = if (h !! i) == '0' then '1' else '0'
                    i = getRandomIndex (getSeed h) h

-- HELPERS
-- helper function to sort the population by s/r descending
sortByAverageScore :: HeuristicPopulation -> HeuristicPopulation
sortByAverageScore hPop = reverse (map snd (sort (getAverageScores hPop)))
-- note that in the case that average scores are the same,
-- this will favour those with higher scores & more rounds
-- see sort behaviour on tuples

-- helper function for above
getAverageScores :: HeuristicPopulation -> [(Int, (Heuristic, (Score, Rounds)))]
getAverageScores hPop = [((s `div` (r+1)), (h, (s, r))) | (h, (s, r)) <- hPop] -- Handles case where heuristic has not been run
