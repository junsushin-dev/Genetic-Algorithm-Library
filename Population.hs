-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin

module Population

where

import BitArrayChromosome -- Arbitrary Chromosome Datatype module
import System.Random

-- type GenChromosome = Rand -> Chromosome
-- function genchromosome of type GenChromosome should be provided by the Chromosome module

-- REQUIRES: a list of Rand variables, the lenght of the list being the number of population you want to generate
-- genBinPop :: GenChromosome -> Int -> Int -> IO [Chromosome]
genBinPop len num = 
    do
        rg <- newStdGen
        let rndStream = randomRs (0, ((2^len)-1) :: Int) rg
        let population = map (dec2bin len) (take num rndStream)
        return population
