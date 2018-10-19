-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin
-- This module illustrates the template for a Cross function
-- If the user wishes to use the template, he/she should specify the functions,
-- -> 'getFst' / 'getSnd' / mkChromosome
-- Otherwise, the user can implement a whole new function following the type definition of function 'cross'

module Cross (
    cross,
    Chromosome
    ) where

import BitArrayChromosome


{- <Test Case>
dad = mkData [0,0,0,0,0,1]
mom = mkData [0,1,0,0,1,1]
cross dad mom
-}

-- cross two chromosomes to make a pair of children
cross::Chromosome -> Chromosome -> (Chromosome, Chromosome)
cross a b = crossTuple (a, b)

crossTuple::(Chromosome, Chromosome) -> (Chromosome, Chromosome)
crossTuple (parent1, parent2) = (child1, child2) where
    child1 = mkChromosome ((getFst parent1)++(getSnd parent2))
    child2 = mkChromosome ((getFst parent2)++(getSnd parent1))