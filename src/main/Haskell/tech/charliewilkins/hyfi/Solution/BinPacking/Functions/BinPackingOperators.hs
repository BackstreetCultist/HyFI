module Solution.BinPacking.Functions.BinPackingOperators where

import HyperHeuristic.Functions.Helpers.RandomOperators (getSeed', getRandomIndex, randomiseList)
import Solution.BinPacking.Types.BinPackingTypes

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

-- Chooses a random item from a random bin, and attempts to place it in a bin currently containing more items than it
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
                                    getSemiRandomisedList seed s = if length (head (s')) < length (head (tail (s'))) then s' else getSemiRandomisedList (seed+1) s
                                                                    where
                                                                        s' = randomiseList seed s

destroyEmpties :: Solution -> Solution
destroyEmpties s = filter (not . null) s
