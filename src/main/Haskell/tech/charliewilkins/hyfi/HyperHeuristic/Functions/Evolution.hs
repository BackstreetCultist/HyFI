module Evolution where

import Data.List (sort)

import HyperHeuristicTypes
import RandomOperators (getSeed, randomiseList, getRandomIndex)
import Application (applyHeuristic)

-- Control function for this section
evolvePopulation :: HeuristicPopulation -> HeuristicPopulation
evolvePopulation hPop = survivalStep hPop (reproductionStep hPop) (mutationStep hPop)
-- Produces a new population of length equal to the original
-- Of the heuristics with the best average performance

-- SURVIVAL
survivalStep :: HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation
survivalStep hPop childs mutoids = replaceWorst hPop childs mutoids

replaceWorst :: HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation -> HeuristicPopulation
replaceWorst hPop childs mutoids = (drop (length childs + length mutoids) (reverse (sortByAverageScore hPop))) ++ childs ++ mutoids

-- REPRODUCTION
reproductionStep :: HeuristicPopulation -> HeuristicPopulation
reproductionStep hPop = applyChildren (generateChildren (selectParents hPop))

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

generateChildren :: (Heuristic, Heuristic) -> (Heuristic, Heuristic)
generateChildren (p1, p2) = kPointCrossover (p1, p2) 1 (getSeed p1)

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

--Produces a HeuristicPopulation of length 2 after running each child once
applyChildren :: (Heuristic, Heuristic) -> HeuristicPopulation
applyChildren (c1, c2) = [(c1, (applyHeuristic c1, 1)), (c2, (applyHeuristic c2, 1))]

-- MUTATION
mutationStep :: HeuristicPopulation -> HeuristicPopulation
mutationStep hPop = applyMutoid (mutateHeuristic (selectHeuristicToMutate hPop))

selectHeuristicToMutate :: HeuristicPopulation -> Heuristic
selectHeuristicToMutate hPop = fst (hPop !! (getRandomIndex (getSeed (fst (head hPop))) hPop))

mutateHeuristic :: Heuristic -> Heuristic
mutateHeuristic h = flipRandomBit h

flipRandomBit :: Heuristic -> Heuristic
flipRandomBit h = take i h ++ [bit] ++ drop (i+1) h
                where
                    bit = if (h !! i) == '0' then '1' else '0'
                    i = getRandomIndex (getSeed h) h

applyMutoid :: Heuristic -> HeuristicPopulation
applyMutoid m = [(m, (applyHeuristic m, 1))]

-- HELPERS
-- helper function to sort the population by s/r descending
sortByAverageScore :: HeuristicPopulation -> HeuristicPopulation
sortByAverageScore hPop = reverse (map snd (sort (getAverageScores hPop)))
-- note that in the case that average scores are the same,
-- this will favour those with higher scores & more rounds
-- see sort behaviour on tuples

-- helper function for above
getAverageScores :: HeuristicPopulation -> [(Int, (Heuristic, (Score, Rounds)))]
getAverageScores hPop = [((s `div` r), (h, (s, r))) | (h, (s, r)) <- hPop]
