module ProblemModules.BinPacking.Functions.Operators where

import Data.List (sortBy)

import Helpers.RandomOperators (getSeed', getRandomIndex, randomiseList)
import ProblemModules.BinPacking.Types

-- Chooses a random item from a random bin, and attempts to place it into a new random bin
randomPlace :: Operator
randomPlace s 0 _ = destroyEmpties s
randomPlace s m i = if solutionValid s' i then randomPlace s' (m-1) i else randomPlace s (m-1) i
                    where
                        s' = randomMove (getSeed' + sum (head s)) s

randomMove :: Int -> Solution -> Solution
randomMove seed s = tail (tail s') ++ (\x -> [fst x, snd x]) (randomMoveBetween (seed+1) (head s') (head (tail s')))
                    where
                        s' = randomiseList seed s

-- Moves a random item from first list to second
randomMoveBetween :: Int -> [Int] -> [Int] -> ([Int], [Int])
randomMoveBetween s xs ys = (tail xs', head xs' : ys')
                            where
                                xs' = randomiseList s xs
                                ys' = randomiseList (s+1) ys

-- Chooses a random item from a random bin, and attempts to place it in a bin currently containing the same or more items than it
maximisingPlace :: Operator
maximisingPlace s 0 _ = destroyEmpties s
maximisingPlace s m i = if solutionValid s' i then maximisingPlace s' (m-1) i else maximisingPlace s (m-1) i
                        where
                            s' = maximisingMove (getSeed' + sum (head s)) s

maximisingMove :: Int -> Solution -> Solution
maximisingMove seed s = tail (tail s') ++ (\x -> [fst x, snd x]) (randomMoveBetween (seed+1) (head s') (head (tail s')))
                        where
                            s' = getSemiRandomisedList seed s
                                where
                                    getSemiRandomisedList seed s = if length (head (s')) <= length (head (tail (s'))) then s' else getSemiRandomisedList (seed+1) s
                                                                    where
                                                                        s' = randomiseList seed s

repackLowestFilled :: Operator
repackLowestFilled s 0 _ = destroyEmpties s
repackLowestFilled s m i = repackLowestFilled (bestFit (head s') (tail s') i) (m-1) i
                        where s' = reverse (sortBy (\e1 e2 -> compare (length e2) (length e1)) s)

destroyHighestBins :: Operator
destroyHighestBins s m i = destroyEmpties (bestFit (concat (take m s')) (drop m s') i)
                        where s' = sortBy (\e1 e2 -> compare (length e2) (length e1)) s

destroyLowestBins :: Operator
destroyLowestBins s m i = destroyEmpties (bestFit (concat (take m s')) (drop m s') i)
                        where s' = reverse (sortBy (\e1 e2 -> compare (length e2) (length e1)) s)

destroyRandomBins :: Operator
destroyRandomBins s m i = destroyEmpties (bestFit (concat (take m s')) (drop m s') i)
                        where s' = randomiseList (getSeed' + sum (head s)) s

bestFit :: [Int] -> Solution -> Instance -> Solution
bestFit [] s _ = s
bestFit (x:xs) s i = bestFit xs (bestFitItem x (sortBy (\e1 e2 -> compare (length e2) (length e1)) s) i) i

-- Expects an item to place, and a sorted list to place it in
bestFitItem :: Int -> Solution -> Instance -> Solution
bestFitItem x [] _ = [[x]]
bestFitItem x (b:bs) i | ((sum (map (\z -> (snd i) !! z) b)) + ((snd i) !! x)) > (fst i) = b : bestFitItem x bs i
                       | otherwise = (b ++ [x]) : bs

destroyEmpties :: Solution -> Solution
destroyEmpties s = filter (not . null) s
