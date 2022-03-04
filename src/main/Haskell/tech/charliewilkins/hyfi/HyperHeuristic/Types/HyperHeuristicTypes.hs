module HyperHeuristic.Types.HyperHeuristicTypes where

type Rounds = Int
type Score = Int
type Heuristic = [Char]
type HeuristicPopulation = [(Heuristic, (Score, Rounds))]
