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

