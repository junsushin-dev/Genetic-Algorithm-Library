-- CPSC 312 - 2018 - Genetic Algorithm Library
-- by Junsu Shin
-- Module: BitArrayFit
-- This module specifies an example of the fitness function for a BitArray type Chromosome of length 6 

module BitArrayFit

where

import BitArrayChromosome

-- functions of type Fit takes a chromosome and evaluates its fitness (represented in Int)
type Fit = Chromosome -> Int

-- <Test Case>
{-
d1 = mkData [0,0,1,0,0,0]
d3 = mkData [0,1,0,1,0,1]
d4 = mkData [1,0,1,0,1,1]
d6 = mkData [1,1,1,1,1,1]
fit d1
fit d3
fit d4
fit d6
-}

-- Sample Target
temptarget = mkData [1,1,1,1,1,1]

-- Fitness function template for a BitArray Chromosome that tries to match the genes with a target
fit::Chromosome -> Int
fit chromosome = targetCompare target chromosome

-- Compares two lists and gives the number of 
targetCompare:: (Eq a) => [a] -> [a] -> Int
targetCompare _ [] = 0
targetCompare [] _ = 0
targetCompare (h1:t1) (h2:t2)
    | h1 == h2 = 1 + targetCompare t1 t2
    | otherwise = targetCompare t1 t2