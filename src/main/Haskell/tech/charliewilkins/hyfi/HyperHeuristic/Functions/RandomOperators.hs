module RandomOperators where

import System.Random (randomR, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Time.Clock.POSIX (getPOSIXTime)

import HyperHeuristicTypes

-- Get seed with heuristic value as denary + current time
getSeed :: Heuristic -> Int
getSeed h = heuristicToSeed h + unsafePerformIO posixNanosInt

-- Get seed without heuristic value
getSeed' :: Int
getSeed' = unsafePerformIO posixNanosInt

-- Used to get seeds for random generators
-- Since a heuristic is a binary string, we can take its denary value
heuristicToSeed :: Heuristic -> Int
heuristicToSeed = foldl' (\acc x -> acc * 2 + digitToInt x) 0
-- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell

posixNanosInt :: IO (Int)
posixNanosInt = do
                x <- getPOSIXTime
                let y = (x*(10^9))
                let z = floor y
                return z

-- This is a quick implementation of the Fisher-Yates Shuffle
randomiseList :: Int -> [a] -> [a]
randomiseList _ [] = []
randomiseList seed xs = xs !! i : randomiseList (seed-1) (take i xs ++ drop (i+1) xs)
                    where
                        i = getRandomIndex seed xs

getRandomIndex :: Int -> [a] -> Int
getRandomIndex seed xs = fst (randomR (0, ((length xs)-1)) (mkStdGen seed))
