-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin
-- This module is a template for a mutate function that should be provided for the user
-- User should specify the 'mutategene' function or implement a new 'mutate' function of his own

module Mutate

where

import BitArrayChromosome
import System.Random
import Data.List

-- mutate a single Chromosome according to pm(mutation probability)
-- REQUIRES: mutategene fuction, Rand type definition
mutate :: Chromosome -> Rand -> Chromosome
mutate ch index = (mutategene ch index)

