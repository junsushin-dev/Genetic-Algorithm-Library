-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin
-- Module: Population
-- This module generates a BitArray type Chromosome population of arbitrary length / number

module Population

where

import BitArrayChromosome -- Arbitrary Chromosome Datatype module
import System.Random

-- <Test Case>
-- genBinPop 10 20

-- function genchromosome of type GenChromosome should be provided by the Chromosome module
genBinPop len num = 
    do
        rg <- newStdGen
        let rndStream = randomRs (0, ((2^len)-1) :: Int) rg
        let population = map (dec2bin len) (take num rndStream)
        return population
