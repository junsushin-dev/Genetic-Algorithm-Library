-- CPSC 312 - 2018 - Genetic Algorithm Library

module Mutate

where

import BitArrayChromosome
import System.Random
import Data.List

-- mutate a single Chromosome according to pm(mutation probability)
-- REQUIRES: mutategene fuction 
mutate :: Chromosome -> Int -> Chromosome
mutate ch index = (mutategene ch index)

-- Replace the nth element in a list by newVal
replaceNth :: [a] -> a -> Int -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) newVal n
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth xs newVal (n-1)

